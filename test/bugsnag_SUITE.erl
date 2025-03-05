-module(bugsnag_SUITE).
-compile([export_all, nowarn_export_all]).

-behaviour(ct_suite).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-spec all() -> [ct_suite:ct_test_def()].
all() ->
    [
        {group, app},
        {group, logger}
    ].

-spec groups() -> [ct_suite:ct_group_def()].
groups() ->
    [
        {app, [sequence], app_tests()},
        {logger, [sequence], [
            can_start_and_stop_the_default_logger,
            can_start_and_stop_different_loggers,
            supervision_tree_starts_successfully,
            does_not_crash_on_bad_messages,
            process_traps_exits,
            all_log_events_can_be_handled
        ]}
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
init_per_group(logger, Config) ->
    application:set_env(bugsnag_erlang, enabled, false),
    {ok, _} = application:ensure_all_started([bugsnag_erlang]),
    Config.

-spec end_per_group(ct_suite:ct_groupname(), ct_suite:ct_config()) -> term().
end_per_group(_, _Config) ->
    application:stop(bugsnag_erlang),
    ok.

-spec init_per_testcase(ct_suite:ct_testcase(), ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_testcase(Name, Config) ->
    case lists:member(Name, app_tests()) of
        true ->
            ok;
        false ->
            meck:new(bugsnag_worker, [no_link, passthrough]),
            meck:expect(bugsnag_worker, deliver_payload, fun(_) -> ok end)
    end,
    Config.

-spec end_per_testcase(ct_suite:ct_testcase(), ct_suite:ct_config()) -> term().
end_per_testcase(Name, _Config) ->
    case lists:member(Name, app_tests()) of
        true ->
            application:unset_env(bugsnag_erlang, enabled),
            application:unset_env(bugsnag_erlang, api_key),
            application:stop(bugsnag_erlang);
        false ->
            meck:unload(bugsnag_worker)
    end,
    ok.

app_tests() ->
    [
        can_start_app_with_disabled,
        can_start_app_with_enabled,
        fails_to_start_with_wrong_api_key,
        can_start_with_different_release_states
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
can_start_and_stop_the_default_logger(_) ->
    Config = #{
        name => ?FUNCTION_NAME,
        pool_size => 1,
        api_key => <<"dummy">>,
        release_stage => production
    },
    {ok, _Sup} = bugsnag:add_handler(Config),
    ct:pal("We can stop any one without affecting the others"),
    bugsnag:remove_handler(?FUNCTION_NAME),
    ?assertMatch({error, _}, logger:get_handler_config(?FUNCTION_NAME)),
    {comment, "Successfully stopped loggers independently of each other."}.

-spec can_start_and_stop_different_loggers(ct_suite:ct_config()) -> term().
can_start_and_stop_different_loggers(_) ->
    Config0 = #{pool_size => 1, api_key => <<"dummy">>, release_stage => production},
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
supervision_tree_starts_successfully(_) ->
    Config = #{
        name => ?FUNCTION_NAME,
        pool_size => 12,
        api_key => <<"dummy">>,
        release_stage => production
    },
    Res = bugsnag:add_handler(Config),
    ?assertMatch({ok, Pid} when is_pid(Pid), Res),
    ?assertNotEqual(undefined, ets:info(?FUNCTION_NAME)).

-spec does_not_crash_on_bad_messages(ct_suite:ct_config()) -> term().
does_not_crash_on_bad_messages(_) ->
    Config = #{
        name => ?FUNCTION_NAME,
        pool_size => 1,
        api_key => <<"dummy">>,
        release_stage => production
    },
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
process_traps_exits(_) ->
    Config = #{
        name => ?FUNCTION_NAME,
        pool_size => 1,
        api_key => <<"dummy">>,
        release_stage => production
    },
    _ = bugsnag:add_handler(Config),
    Pid = get_worker(?FUNCTION_NAME),
    ct:pal("Verifiying that the process won't be killed by exits:"),
    exit(Pid, shutdown),
    ?assert(is_process_alive(Pid)),
    {comment, "Process successfully survived exit signals"}.

-spec all_log_events_can_be_handled(ct_suite:ct_config()) -> term().
all_log_events_can_be_handled(_) ->
    Config = #{
        name => ?FUNCTION_NAME,
        pool_size => 1,
        api_key => <<"dummy">>,
        release_stage => production
    },
    _ = bugsnag:add_handler(Config),
    Pid = get_worker(?FUNCTION_NAME),
    generate_all_log_level_events_and_types(),
    assert_all_log_levels_were_triggered(Pid),
    {comment, "All log levels were triggered and successfully stored in DB."}.

%% Helpers
-spec generate_all_log_level_events_and_types() -> term().
generate_all_log_level_events_and_types() ->
    ct:pal("Attempt to log each severity level in different ways"),
    logger:emergency("log_all_events: some log"),
    ?LOG_ALERT("log_all_events: some logs ~p", [value]),
    logger:critical("log_all_events: some log ~p", [critical]),
    error_logger:warning_msg("log_all_events: old error logger"),
    logger:warning([{what, #{}}]),
    ?LOG_NOTICE(#{what => notice_this, reason => just_because, tag => log_all_events}),
    logger:log(info, #{what => information, context => #{from => log_all_events}}),
    ?LOG_DEBUG("log_all_events: a debug log ~p", [#{debug => true}]).

-spec get_worker(atom()) -> pid().
get_worker(Name) ->
    ets:lookup_element(Name, 1, 2).

-spec get_all_delivered_payloads(pid()) -> [term()].
get_all_delivered_payloads(Pid) ->
    Logs = meck:history(bugsnag_worker, Pid),
    ct:pal("All logged events ~p~n", [Logs]),
    Logs.

-spec assert_all_log_levels_were_triggered(pid()) -> boolean().
assert_all_log_levels_were_triggered(Pid) ->
    Logs = get_all_delivered_payloads(Pid),
    ?assert(7 < length(Logs)).

-spec assert_has_logs(pid(), atom()) -> ok.
assert_has_logs(Pid, FunctionName) ->
    HasFunctionName = lists:any(
        fun(Log) ->
            atom_to_binary(FunctionName) =:= lists:nth(6, Log)
        end,
        get_all_delivered_payloads(Pid)
    ),
    ?assert(HasFunctionName).

-spec assert_has_no_logs(pid(), atom()) -> ok.
assert_has_no_logs(Pid, FunctionName) ->
    HasFunctionName = lists:all(
        fun(Log) ->
            atom_to_binary(FunctionName) =/= lists:nth(6, Log)
        end,
        get_all_delivered_payloads(Pid)
    ),
    ?assert(HasFunctionName).
