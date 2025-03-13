-module(bugsnag_SUITE).
-compile([export_all, nowarn_export_all]).

-behaviour(ct_suite).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-spec all() -> [ct_suite:ct_test_def()].
all() ->
    [
        {group, app},
        {group, logger},
        {group, log_messages}
    ].

-spec groups() -> [ct_suite:ct_group_def()].
groups() ->
    [
        {app, [sequence], app_tests()},
        {logger, [sequence], logger_tests()},
        {log_messages, [sequence], log_messages_tests()}
    ].

-spec init_per_suite(ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_suite(Config) ->
    PrimaryLoggerConfig = logger:get_primary_config(),
    logger:update_primary_config(#{level => all}),
    [{primary_logger_config, PrimaryLoggerConfig} | Config].

-spec end_per_suite(ct_suite:ct_config()) -> term().
end_per_suite(Config) ->
    PrimaryLoggerConfig = proplists:get_value(primary_logger_config, Config),
    logger:set_primary_config(PrimaryLoggerConfig),
    Config.

-spec init_per_group(ct_suite:ct_groupname(), ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_group(app, Config) ->
    Config;
init_per_group(_, Config) ->
    application:set_env(bugsnag_erlang, enabled, false),
    {ok, _} = application:ensure_all_started([bugsnag_erlang]),
    Config.

-spec end_per_group(ct_suite:ct_groupname(), ct_suite:ct_config()) -> term().
end_per_group(_, _Config) ->
    application:stop(bugsnag_erlang),
    ok.

-spec init_per_testcase(ct_suite:ct_testcase(), ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_testcase(_Name, Config) ->
    Ref = make_ref(),
    Port = http_helper:start('_', process_request(Ref)),
    [{port, Port}, {ref, Ref} | Config].

-spec end_per_testcase(ct_suite:ct_testcase(), ct_suite:ct_config()) -> term().
end_per_testcase(Name, _Config) ->
    http_helper:stop(),
    case lists:member(Name, app_tests()) of
        true ->
            application:stop(bugsnag_erlang),
            application:unset_env(bugsnag_erlang, enabled),
            application:unset_env(bugsnag_erlang, api_key),
            application:unset_env(bugsnag_erlang, release_stage);
        false ->
            ok
    end.

app_tests() ->
    [
        can_start_app_with_disabled,
        can_start_app_with_enabled,
        fails_to_start_with_wrong_api_key,
        can_start_with_different_release_states
    ].

logger_tests() ->
    [
        can_start_and_stop_the_default_logger,
        can_start_and_stop_different_loggers,
        supervision_tree_starts_successfully,
        does_not_crash_on_bad_messages,
        process_traps_exits
    ].

log_messages_tests() ->
    [
        generate_exception_event_from_structured_log,
        all_log_events_can_be_handled,
        verify_against_schema
    ].

%% Tests
-spec can_start_app_with_disabled(ct_suite:ct_config()) -> term().
can_start_app_with_disabled(_) ->
    application:set_env(bugsnag_erlang, enabled, false),
    {ok, _} = application:ensure_all_started([bugsnag_erlang]),
    ?assertEqual(
        true,
        lists:all(
            fun({_, Count}) -> Count =:= 0 end,
            supervisor:count_children(bugsnag_sup)
        )
    ).

-spec can_start_app_with_enabled(ct_suite:ct_config()) -> term().
can_start_app_with_enabled(_) ->
    application:set_env(bugsnag_erlang, enabled, true),
    List = [<<"dummy">>, "dummy"],
    Fun = fun(ApiKey) ->
        application:set_env(bugsnag_erlang, api_key, ApiKey),
        {ok, _} = application:ensure_all_started([bugsnag_erlang]),
        Res = supervisor:count_children(bugsnag_sup),
        ?assert(lists:any(fun({_, Count}) -> Count =:= 1 end, Res), Res),
        application:stop(bugsnag_erlang)
    end,
    lists:foreach(Fun, List).

-spec fails_to_start_with_wrong_api_key(ct_suite:ct_config()) -> term().
fails_to_start_with_wrong_api_key(_) ->
    application:set_env(bugsnag_erlang, enabled, true),
    List = ["ENTER_API_KEY", undefined],
    Fun = fun(ApiKey) ->
        application:set_env(bugsnag_erlang, api_key, ApiKey),
        ?assertMatch({error, _}, application:ensure_all_started([bugsnag_erlang]))
    end,
    lists:foreach(Fun, List).

-spec can_start_with_different_release_states(ct_suite:ct_config()) -> term().
can_start_with_different_release_states(_) ->
    application:set_env(bugsnag_erlang, enabled, true),
    application:set_env(bugsnag_erlang, api_key, <<"dummy">>),
    List = [development, staging, production, test, "production"],
    Fun = fun(ReleaseState) ->
        application:set_env(bugsnag_erlang, release_state, ReleaseState),
        {ok, _} = application:ensure_all_started([bugsnag_erlang]),
        Res = supervisor:count_children(bugsnag_sup),
        ?assert(lists:any(fun({_, Count}) -> Count =:= 1 end, Res), Res),
        application:stop(bugsnag_erlang)
    end,
    lists:foreach(Fun, List).

-spec can_start_and_stop_the_default_logger(ct_suite:ct_config()) -> term().
can_start_and_stop_the_default_logger(CtConfig) ->
    {ok, _Sup} = bugsnag:add_handler(template_handler(CtConfig, ?FUNCTION_NAME)),
    ct:pal("We can stop any one without affecting the others"),
    bugsnag:remove_handler(?FUNCTION_NAME),
    ?assertMatch({error, _}, logger:get_handler_config(?FUNCTION_NAME)),
    {comment, "Successfully stopped loggers independently of each other."}.

-spec can_start_and_stop_different_loggers(ct_suite:ct_config()) -> term().
can_start_and_stop_different_loggers(CtConfig) ->
    Config0 = template_handler(CtConfig, ?FUNCTION_NAME),
    ct:pal("After starting three loggers"),
    ConfigA = Config0#{name => config_a},
    ConfigB = Config0#{name => config_b},
    ConfigC = Config0#{name => config_c},
    {ok, _} = bugsnag:add_handler(ConfigA),
    {ok, _} = bugsnag:add_handler(ConfigB),
    {ok, _} = bugsnag:add_handler(ConfigC),
    ct:pal("We can stop any one without affecting the others"),
    bugsnag:remove_handler(ConfigC),
    ?assertMatch({error, _}, logger:get_handler_config(config_c)),
    ?assertMatch({ok, _}, logger:get_handler_config(config_b)),
    ?assertMatch({ok, _}, logger:get_handler_config(config_a)),
    bugsnag:remove_handler(ConfigB),
    ?assertMatch({error, _}, logger:get_handler_config(config_c)),
    ?assertMatch({error, _}, logger:get_handler_config(config_b)),
    ?assertMatch({ok, _}, logger:get_handler_config(config_a)),
    bugsnag:remove_handler(ConfigA),
    ?assertMatch({error, _}, logger:get_handler_config(config_c)),
    ?assertMatch({error, _}, logger:get_handler_config(config_b)),
    ?assertMatch({error, _}, logger:get_handler_config(config_a)),
    {comment, "Successfully stopped loggers independently of each other."}.

-spec supervision_tree_starts_successfully(ct_suite:ct_config()) -> term().
supervision_tree_starts_successfully(CtConfig) ->
    Res = bugsnag:add_handler(template_handler(CtConfig, ?FUNCTION_NAME)),
    ?assertMatch({ok, Pid} when is_pid(Pid), Res),
    ?assertNotEqual(undefined, ets:info(?FUNCTION_NAME)).

-spec does_not_crash_on_bad_messages(ct_suite:ct_config()) -> term().
does_not_crash_on_bad_messages(CtConfig) ->
    Config = template_handler(CtConfig, ?FUNCTION_NAME),
    _ = bugsnag:add_handler(Config),
    ct:pal("Sending different invalid messages to the process..."),
    bugsnag_worker:notify_worker(Config, #{something => <<"misterious">>}),
    bugsnag_worker:notify_worker(Config, #{ref => make_ref()}),
    Pid = get_worker(?FUNCTION_NAME),
    gen_server:cast(Pid, #{ref => make_ref()}),
    gen_server:call(Pid, #{ref => make_ref()}),
    Pid ! random_message,
    ct:pal("We can verify that the process is still alive."),
    ?assert(is_process_alive(Pid)),
    {comment, "Process successfully survived invalid messages."}.

-spec process_traps_exits(ct_suite:ct_config()) -> term().
process_traps_exits(CtConfig) ->
    _ = bugsnag:add_handler(template_handler(CtConfig, ?FUNCTION_NAME)),
    Pid = get_worker(?FUNCTION_NAME),
    ct:pal("Verifiying that the process won't be killed by exits:"),
    exit(Pid, shutdown),
    ?assert(is_process_alive(Pid)),
    {comment, "Process successfully survived exit signals"}.

-spec generate_exception_event_from_structured_log(ct_suite:ct_config()) -> term().
generate_exception_event_from_structured_log(CtConfig) ->
    _ = bugsnag:add_handler(template_handler(CtConfig, ?FUNCTION_NAME)),
    generate_exception(bugsnag_gen_trace),
    {ok, Logs} = wait_until_at_least_logs(CtConfig, 1),
    Pred = fun
        (
            #{
                <<"events">> := [
                    #{
                        <<"exceptions">> := [
                            #{
                                <<"errorClass">> := <<"throw">>,
                                <<"message">> := <<"bugsnag_gen_trace">>
                            }
                        ]
                    }
                ]
            }
        ) ->
            true;
        (_) ->
            false
    end,
    Result = lists:any(Pred, Logs),
    ?assert(Result, Logs),
    verify_schema(schema(), Logs),
    {comment, "Generated exception formatted correctly"}.

-spec all_log_events_can_be_handled(ct_suite:ct_config()) -> term().
all_log_events_can_be_handled(CtConfig) ->
    _ = bugsnag:add_handler(template_handler(CtConfig, ?FUNCTION_NAME)),
    generate_all_log_level_events_and_types(),
    case wait_until_at_least_logs(CtConfig, 8) of
        {ok, Logs} ->
            verify_schema(schema(), Logs),
            {comment, "All log levels were triggered."};
        {error, {_, _, [{times, _, History} | _], _}} ->
            ct:fail("Failed with last check having ~B logs (expected ~B)", [length(History), 8])
    end.

-spec verify_against_schema(ct_suite:ct_config()) -> term().
verify_against_schema(CtConfig) ->
    _ = bugsnag:add_handler(template_handler(CtConfig, ?FUNCTION_NAME)),
    generate_exception(bugsnag_gen_trace),
    Logs = get_all_delivered_payloads(CtConfig),
    Schema = schema(),
    verify_schema(Schema, Logs),
    {comment, "All returned errors are validated against the schema"}.

-spec verify_schema(map(), [map()]) -> any().
verify_schema(Schema, Entries) ->
    [
        ?assertMatch({ok, _}, jesse:validate_with_schema(Schema, Data))
     || Data <- Entries
    ].

%% Helpers
-spec generate_all_log_level_events_and_types() -> term().
generate_all_log_level_events_and_types() ->
    logger:emergency("1. log_all_events: some log"),
    ?LOG_ALERT("2. log_all_events: some logs ~p", [value]),
    logger:critical("3. log_all_events: some log ~p", [critical]),
    error_logger:warning_msg("4. log_all_events: old error logger"),
    logger:warning([{what, warning_with_list_of_tuples}, {num, 5}]),
    ?LOG_NOTICE(#{what => notice_this_SUITE, num => 6, tag => log_all_events}),
    logger:log(info, #{what => information_SUITE, num => 7, context => #{from => log_all_events}}),
    ?LOG_DEBUG("8. log_all_events: a debug log _SUITE ~p", [#{debug => true}]).

-spec get_worker(atom()) -> pid().
get_worker(Name) ->
    ets:lookup_element(Name, 1, 2).

-spec get_all_delivered_payloads(ct_suite:ct_config()) -> [term()].
get_all_delivered_payloads(CtConfig) ->
    {ref, Ref} = lists:keyfind(ref, 1, CtConfig),
    Logs0 = receive_all(Ref, []),
    Logs = lists:flatmap(
        fun(#{<<"events">> := Events} = Log) ->
            [Log#{<<"events">> := [Event]} || Event <- Events]
        end,
        Logs0
    ),
    ct:pal("All logged events ~p~n", [Logs]),
    Logs.

process_request(Ref) ->
    Pid = self(),
    fun(Req0) ->
        {ok, Data, Req} = cowboy_req:read_body(Req0, #{length => 8000000, period => 5000}),
        Pid ! {Ref, Data},
        cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"OK">>, Req)
    end.

receive_all(Ref, Acc) ->
    receive
        {Ref, Data} ->
            receive_all(Ref, [json:decode(Data) | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

-spec wait_until_at_least_logs(ct_suite:ct_config(), non_neg_integer()) -> [term()].
wait_until_at_least_logs(CtConfig, Num) ->
    Validator = fun(ReturnValue) -> Num =< length(ReturnValue) end,
    wait_helper:wait_until(
        fun() -> get_all_delivered_payloads(CtConfig) end,
        expected,
        #{no_throw => true, time_left => timer:seconds(1), sleep_time => 50, validator => Validator}
    ).

-spec assert_has_logs(ct_suite:ct_config(), atom()) -> ok.
assert_has_logs(CtConfig, FunctionName) ->
    HasFunctionName = lists:any(
        fun(Log) ->
            atom_to_binary(FunctionName) =:= lists:nth(6, Log)
        end,
        get_all_delivered_payloads(CtConfig)
    ),
    ?assert(HasFunctionName).

-spec assert_has_no_logs(ct_suite:ct_config(), atom()) -> ok.
assert_has_no_logs(CtConfig, FunctionName) ->
    HasFunctionName = lists:all(
        fun(Log) ->
            atom_to_binary(FunctionName) =/= lists:nth(6, Log)
        end,
        get_all_delivered_payloads(CtConfig)
    ),
    ?assert(HasFunctionName).

template_handler(CtConfig, Name) ->
    {port, Port} = lists:keyfind(port, 1, CtConfig),
    #{
        name => Name,
        pool_size => 1,
        api_key => <<"dummy">>,
        release_stage => production,
        endpoint => "http://localhost:" ++ integer_to_list(Port)
    }.

generate_exception(Reason0) ->
    try
        throw(Reason0)
    catch
        Class:Reason:StactTrace ->
            ?LOG_ERROR(#{
                what => generate_exception,
                class => Class,
                reason => Reason,
                stacktrace => StactTrace
            })
    end.

schema() ->
    json:decode(list_to_binary(raw_schema())).

raw_schema() ->
    """
    {
     "$schema": "http://json-schema.org/draft-06/schema#",
     "type": "object",
     "properties": {
       "apiKey": {
         "type": "string"
        },
       "payloadVersion": {
               "enum": [5]
        },
       "notifier": {
         "type": "object",
         "properties": {
           "name": {
             "type": "string"
            },
           "version": {
             "type": "string"
            },
           "url": {
             "type": "string"
            },
           "dependencies": {
             "type": "array"
            }
          },
         "required": ["name", "version", "url"]
        },
       "events": {
         "type": "array",
         "items": {
           "type": "object",
           "properties": {
             "severity": {
               "enum": ["error", "warning", "info"]
              },
             "severityReason": {
               "type": "object",
               "properties": {
                 "type": {"type": "string"},
                 "attributes": {"type": "object"}
                }
              },
             "context": {
               "type": "string"
              },
             "metaData": {
               "type": "object"
              },
              "breadcrumbs": {
                "type": "array",
                "items": {
                  "type": "object",
                  "properties": {
                    "timestamp": { "type": "string" },
                    "name": { "type": "string" },
                    "type": { "type": "string" },
                    "metaData": { "type": "object" }
                  },
                  "required": ["timestamp", "name", "type"]
                }
              },
             "exceptions": {
               "type": "array",
               "items": {
                 "type": "object",
                 "properties": {
                   "type": {
                     "enum": [
                         "android", "browser", "js", "c", "cocoa", "csharp", "electron",
                         "node", "js", "electronrendererjs", "expojs", "go", "java",
                         "nodejs", "php", "python", "reactnativejs", "ruby"
                     ]
                    },
                   "message": {
                     "type": "string"
                    },
                   "errorClass": {
                     "type": "string"
                    },
                   "stacktrace": {
                     "type": "array",
                     "items": {
                       "type": "object",
                       "properties": {
                         "file": {
                           "type": "string"
                          },
                         "lineNumber": {
                           "type": "integer"
                          },
                         "method": {
                           "type": "string"
                          }
                        },
                       "required": ["file", "lineNumber", "method"]
                      }
                    }
                  },
                 "required": ["errorClass", "stacktrace"]
                }
              }
            },
           "required": ["exceptions"]
          }
        }
      },
     "required": ["payloadVersion", "notifier", "events"]
    }
    """.
