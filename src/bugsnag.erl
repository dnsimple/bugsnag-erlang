-module(bugsnag).
-moduledoc """
BugSnag Erlang client.

## Configuration
It can be configured using application environment variables or by passing a map to the
`start_link/1` function.

If using application environment variables, the config looks like the following:
```erlang
{bugsnag_erlang, [
    {enabled, true},
    {api_key, "BUGSNAG_API_KEY"},
    {release_stage, "production"},
    {handler_name, bugsnag_logger_handler},
    {pool_size, 10}
]}
```
If `enabled` is set to other than true, the rest of the configuration won't be read
and no handler will be registered. For the rest of the keys, see `t:config/0`.

If a new handler wants to be added, the `handler_name` key can be set to a new atom.
""".

-export([start_link/2, add_handler/1, add_handler/2, remove_handler/1]).
-export([notify/3]).
-export([notify/5, notify/7]).
-deprecated([{start_link, 2, next_major_release}]).
-deprecated([{notify, 5, next_major_release}]).
-deprecated([{notify, 7, next_major_release}]).

-doc """
Configuration options for the Bugsnag client.

It takes the following configuration options:

- `api_key`: the Bugsnag API key, mandatory to provide.
- `release_stage`: the release stage of the application, defaults to `production`
- `name`: defaults to `bugsnag_logger_handler`, but allows to create more `logger` handlers
    with different configurations.
- `pool_size`: defaults to the number of schedulers, increases the number of workers in the pool
    in case of high load.
- `endpoint`: allows you to configure a custom config, including testing endpoints.
- `events_limit`: allows you to configure the maximum number of events to be sent to Bugsnag at
    once. If event queues fill too fast, older non-sent events will be dropped.
    The default is 1000, as an average big event including stacktraces is no bigger than 1KB,
    therefore 1000 events should keep the payload below 1MB.
- `notifier_name`: a string, the notifier name as required by BugSnag. Defaults to "Bugsnag Erlang".
""".
-type config() :: #{
    api_key := binary(),
    release_stage := binary(),
    name := logger_handler:id(),
    pool_size := pos_integer(),
    endpoint => binary(),
    events_limit => pos_integer(),
    notifier_name => binary()
}.

-doc "A printable string".
-type text() :: atom() | string() | binary().

-doc """
Event to notify to BugSnag.

1. Events that contain `#{class := _, reason := _, stacktrace := _}`, following the naming
    convention as exemplified by `erlang:raise/3`, will be treated as exceptions to BugSnag;
    or `#{kind := _, reason := _, stacktrace := _}` as per `telemetry`'s convention;
    or `#{error := #{kind := _, message := _, stack := _}` as per
    [DataDog](https://docs.datadoghq.com/logs/log_collection/?tab=http#attributes-for-stack-traces)
    convention;
2. Event structure will have all their printable key-value pairs as key-value pairs in `metaData`.
3. `breadcrumbs` will always contain the timestamp of the event.
""".
-type event() :: #{
    what => text(),
    message => text(),
    class => exit | error | throw,
    kind => exit | error | throw,
    error => #{
        kind => exit | error | throw,
        message => term(),
        stacktrace => [{module(), atom(), non_neg_integer() | [term()], [{atom(), _}]}]
    },
    reason => term(),
    stacktrace => [{module(), atom(), non_neg_integer() | [term()], [{atom(), _}]}],
    atom() => text()
}.

-doc """
Metadata about the event.

The default severity level is `warning`.
The default `mfa` is `{undefined, undefined, 0}` and `line` is also 0.
""".
-type metadata() :: #{
    mfa := {module(), atom(), non_neg_integer()},
    line := non_neg_integer(),
    level => logger:level(),
    time => integer(),
    atom() => term()
}.

-export_type([config/0, event/0, metadata/0, text/0]).

-doc "Add a new logger handler.".
-spec add_handler(config()) -> supervisor:startchild_ret().
add_handler(Config) ->
    bugsnag_sup:add_handler(#{config => Config}).

-doc "Add a new logger handler.".
-spec add_handler(config(), logger_handler:config()) -> supervisor:startchild_ret().
add_handler(Config, LoggerConfig) ->
    bugsnag_sup:add_handler(LoggerConfig#{config => Config}).

-doc "Remove a new logger handler.".
-spec remove_handler(logger_handler:id() | config()) -> ok | {error, term()}.
remove_handler(#{name := Name}) ->
    bugsnag_sup:remove_handler(Name);
remove_handler(Name) ->
    bugsnag_sup:remove_handler(Name).

-doc """
Notify of a bugsnag event.

1. Events that contain `#{class := _, reason := _, stacktrace := _}`, following the naming
    convention as exemplified by `erlang:raise/3`, will be treated as exceptions to BugSnag;
    or `#{kind := _, reason := _, stacktrace := _}` as per `telemetry`'s convention;
    or `#{error := #{kind := _, message := _, stack := _}` as per
    [DataDog](https://docs.datadoghq.com/logs/log_collection/?tab=http#attributes-for-stack-traces)
    convention;
2. Event structure will have all their printable key-value pairs as key-value pairs in `metaData`
""".
-spec notify(config(), event(), metadata()) -> term().
notify(BugSnagConfig, Report, Meta) ->
    Event = #{
        severity => build_severity(Meta),
        'severityReason' => build_severity_reason(Meta),
        'metaData' => build_metadata(Report, Meta),
        context => build_context(Report, Meta),
        exceptions => build_exception(Report),
        breadcrumbs => build_breadcrumb(Report, Meta)
    },
    bugsnag_worker:notify_worker(BugSnagConfig, {event, Event}).

-doc """
Add a new global `bugsnag_logger_handler` handler.

This is deprecated, `add_handler/1` is preferred.
""".
-spec start_link(binary(), binary()) -> gen_server:start_ret().
start_link(ApiKey, ReleaseStage) ->
    bugsnag_worker:start_link(#{
        name => bugsnag_logger_handler,
        pool_size => 1,
        api_key => ApiKey,
        release_stage => ReleaseStage
    }).

-doc "Notify a global worker about an exception.".
-spec notify(atom(), atom() | string(), string() | binary(), module(), non_neg_integer()) -> ok.
notify(Type, Reason, Message, Module, Line) ->
    notify(Type, Reason, Message, Module, Line, generate_trace(), undefined).

-doc "Notify a global worker about an exception.".
-spec notify(
    atom(), atom() | string(), string() | binary(), module(), non_neg_integer(), [term()], term()
) -> ok.
notify(Type, Reason, Message, Module, Line, Trace, Request) ->
    Payload = #{
        type => Type,
        reason => Reason,
        message => Message,
        module => Module,
        line => Line,
        trace => Trace,
        request => Request
    },
    gen_server:cast(bugsnag_worker, {legacy, Payload}).

-spec generate_trace() -> list().
generate_trace() ->
    StepsBack = 1,
    case erlang:process_info(self(), current_stacktrace) of
        {_, StackTrace} when is_list(StackTrace) ->
            lists:nthtail(StepsBack, StackTrace);
        _ ->
            try
                throw(bugsnag_gen_trace)
            catch
                _:_:StackTrace ->
                    lists:nthtail(StepsBack, StackTrace)
            end
    end.

-spec build_exception(map()) -> [bugsnag_api_error_reporting:exception()].
build_exception(#{class := Class, reason := Reason, stacktrace := StackTrace}) ->
    do_build_exception(Class, Reason, StackTrace);
build_exception(#{kind := Class, reason := Reason, stacktrace := StackTrace}) ->
    do_build_exception(Class, Reason, StackTrace);
build_exception(#{error := #{kind := Class, message := Reason, stack := StackTrace}}) ->
    do_build_exception(Class, Reason, StackTrace);
build_exception(_) ->
    [].

do_build_exception(Class, Reason, StackTrace) ->
    [
        #{
            'errorClass' => Class,
            message => Reason,
            stacktrace => process_trace(StackTrace)
        }
    ].

-spec build_breadcrumb(map(), logger:metadata()) -> [bugsnag_api_error_reporting:breadcrumb()].
build_breadcrumb(Report, #{time := Time}) ->
    TimeString = list_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond}])),
    [
        #{
            timestamp => TimeString,
            name => build_breadcrumb_name(Report),
            type => breadcrumb_type(Report)
        }
    ];
build_breadcrumb(Report, _) ->
    Time = erlang:system_time(microsecond),
    build_breadcrumb(Report, #{time => Time}).

-spec breadcrumb_type(map()) -> bugsnag_api_error_reporting:breadcrumb_type().
%% `erlang:raise/3`
breadcrumb_type(#{class := _, reason := _, stacktrace := _}) ->
    error;
%% `telemetry` exceptions
breadcrumb_type(#{kind := _, reason := _, stacktrace := _}) ->
    error;
%% DataDog errors
breadcrumb_type(#{error := #{kind := _, message := _, stacktrace := _}}) ->
    error;
breadcrumb_type(_) ->
    log.

-spec build_context(map(), logger:metadata()) -> text().
build_context(#{what := What}, _) ->
    What;
build_context(#{message := Message}, _) ->
    Message;
build_context(_, _) ->
    <<>>.

-spec build_breadcrumb_name(map()) -> text().
build_breadcrumb_name(#{what := What}) ->
    What;
build_breadcrumb_name(#{message := Text}) ->
    Text;
build_breadcrumb_name(_) ->
    undefined.

-spec build_severity(metadata()) -> bugsnag_api_error_reporting:severity().
build_severity(#{level := emergency}) -> error;
build_severity(#{level := alert}) -> error;
build_severity(#{level := critical}) -> error;
build_severity(#{level := error}) -> error;
build_severity(#{level := notice}) -> info;
build_severity(#{level := info}) -> info;
build_severity(#{level := debug}) -> info;
build_severity(#{level := warning}) -> warning;
build_severity(_) -> warning.

-spec build_severity_reason(metadata()) -> bugsnag_api_error_reporting:severity_reason().
build_severity_reason(#{level := Level}) ->
    #{type => log, attributes => #{level => Level}};
build_severity_reason(_) ->
    #{type => log, attributes => #{level => warning}}.

-spec build_metadata(map(), logger:metadata()) -> map().
build_metadata(Report, #{mfa := Mfa, line := Line}) ->
    BaseReport = #{K => V || K := V <- Report, is_atom(K), (is_atom(V) orelse is_binary(V))},
    BaseReport#{function_name => function_name(Mfa), line => Line}.

-spec function_name({atom(), atom(), pos_integer()}) -> binary().
function_name({Module, Function, Arity}) ->
    function_name(Module, Function, Arity).

-spec function_name(atom(), atom(), pos_integer()) -> binary().
function_name(Module, Function, Arity) ->
    iolist_to_binary(io_lib:format("~p:~p/~p", [Module, Function, Arity])).

-spec process_trace([tuple()]) -> [bugsnag_api_error_reporting:stackframe()].
process_trace(StackTrace) ->
    process_trace(StackTrace, []).

process_trace([], ProcessedTrace) ->
    lists:reverse(ProcessedTrace);
process_trace([{Mod, Fun, Args, Info} | Rest], ProcessedTrace) when is_list(Args) ->
    Arity = length(Args),
    process_trace([{Mod, Fun, Arity, Info} | Rest], ProcessedTrace);
process_trace([{Mod, Fun, Arity, Info} | Rest], ProcessedTrace) when is_integer(Arity) ->
    LineNum = proplists:get_value(line, Info, 0),
    FunName = function_name(Mod, Fun, Arity),
    File = iolist_to_binary(proplists:get_value(file, Info, "")),
    Trace = #{
        file => File,
        'lineNumber' => LineNum,
        method => FunName
    },
    process_trace(Rest, [Trace | ProcessedTrace]);
process_trace([_Current | Rest], ProcessedTrace) ->
    process_trace(Rest, ProcessedTrace).
