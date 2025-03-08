-module(bugsnag_logger_handler).
-moduledoc """
Logger handler module for BugSnag.

It is specially tailored to structured logging, and therefore adheres to the following structure:

1. Unstructured logging will be treated structurised ad-hoc, with the message being the only field.
2. Events that contain `#{class := _, reason := _, stacktrace := _}`, following the naming
    convention as exemplified by `erlang:raise/3`, will be treated as exceptions to BugSnag.
3. Event structure will have all their printable key-value pairs as key-value pairs in `metaData`
4. Events that contain a `t:logger:report_cb()` will have this printable string as its `context`,
    otherwise, the `what` field will be used.
5. `breadcrumbs` will always contain the timestamp of the event.
""".

-behaviour(logger_handler).

-export([log/2]).

-doc "OTP logger-compliant handler for BugSnag".
-spec log(logger:log_event(), logger_handler:config()) -> any().
log(LogEvent, Config) ->
    catch extract(LogEvent, Config).

-spec extract(logger:log_event(), logger_handler:config()) -> any().
extract(#{level := Level, msg := Msg0, meta := Meta0}, #{config := BugSnagConfig}) ->
    Meta = ensure_mfa(Meta0),
    Report = make_msg_structured(Msg0),
    do_log(Report, Meta, Level, BugSnagConfig).

-spec ensure_mfa(logger:metadata()) -> logger:metadata().
%% Do not infinite-loop logs from the worker module
ensure_mfa(#{mfa := {bugsnag_worker, _, _}}) ->
    throw(ignore_bugsnag_worker_logs);
%% No mfa or line supplied in this one, we'll put in placeholders.
ensure_mfa(Meta) when is_map(Meta), not is_map_key(mfa, Meta) ->
    Meta#{mfa => {undefined, undefined, 0}, line => 0};
ensure_mfa(Meta) ->
    Meta.

-spec make_msg_structured(
    {io:format(), [term()]}
    | {report, logger:report()}
    | {string, unicode:chardata()}
) -> map().
make_msg_structured({report, #{} = Report}) ->
    Report;
make_msg_structured({report, Report}) when is_list(Report) ->
    maps:from_list(Report);
make_msg_structured({string, Message}) ->
    #{unstructured => iolist_to_binary(Message)};
%% Unstructured logging, often from STDLIB or old libs
make_msg_structured({Format, Args}) when is_list(Format), is_list(Args) ->
    #{unstructured => iolist_to_binary(io_lib:format(Format, Args))}.

-spec do_log(map(), logger:metadata(), logger:level(), bugsnag:config()) -> term().
do_log(Report, Meta, Level, BugSnagConfig) ->
    Event = #{
        severity => build_severity(Level),
        severityReason => build_severity_reason(Level),
        metaData => build_metadata(Report, Meta),
        context => build_context(Report, Meta),
        exceptions => build_exception(Report),
        breadcrumbs => build_breadcrumb(Report, Meta)
    },
    bugsnag_worker:notify_worker(BugSnagConfig, {event, Event}).

-spec build_exception(map()) -> [bugsnag_api_error_reporting:exception()].
build_exception(#{class := Class, reason := Reason, stacktrace := StackTrace}) ->
    [
        #{
            'errorClass' => Class,
            message => Reason,
            stacktrace => process_trace(StackTrace)
        }
    ];
build_exception(_) ->
    [].

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
build_breadcrumb(_, _) ->
    [].

-spec breadcrumb_type(map()) -> bugsnag_api_error_reporting:breadcrumb_type().
breadcrumb_type(#{class := _, reason := _, stacktrace := _}) ->
    error;
breadcrumb_type(_) ->
    log.

-spec build_context(map(), logger:metadata()) -> bugsnag_api_error_reporting:text().
build_context(Report, #{report_cb := ReportCallback}) when is_function(ReportCallback) ->
    apply_report_callback(Report, ReportCallback);
build_context(#{what := What}, _) ->
    What;
build_context(#{unstructured := Unstructured}, _) ->
    Unstructured;
build_context(_, _) ->
    <<>>.

-spec build_breadcrumb_name(map()) -> bugsnag_api_error_reporting:text().
build_breadcrumb_name(#{what := What}) ->
    What;
build_breadcrumb_name(#{text := Text}) ->
    Text;
build_breadcrumb_name(#{unstructured := _}) ->
    unstructured;
build_breadcrumb_name(_) ->
    undefined.

-spec build_severity(logger:level()) -> bugsnag_api_error_reporting:severity().
build_severity(emergency) -> error;
build_severity(alert) -> error;
build_severity(critical) -> error;
build_severity(error) -> error;
build_severity(warning) -> warning;
build_severity(_) -> info.

-spec build_severity_reason(logger:level()) -> bugsnag_api_error_reporting:severity_reason().
build_severity_reason(Level) ->
    #{type => log, attributes => #{level => Level}}.

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

%%% Applies a report callback function to a given report.
-spec apply_report_callback(map(), logger:report_cb()) -> binary().
apply_report_callback(Report, ReportCallback) when is_function(ReportCallback, 1) ->
    {Format, Terms} = ReportCallback(Report),
    iolist_to_binary(io_lib:format(Format, Terms));
apply_report_callback(Report, ReportCallback) when is_function(ReportCallback, 2) ->
    DefaultConfig = #{depth => unlimited, chars_limit => 256, single_line => true},
    iolist_to_binary(ReportCallback(Report, DefaultConfig)).
