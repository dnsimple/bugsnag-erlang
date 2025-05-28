-module(bugsnag_logger_handler).
-moduledoc """
Logger handler module for BugSnag.

It is specially tailored to structured logging, and therefore unstructured logging will be treated
structurised ad-hoc, with the message being the only field of the `t:bugsnag:event/0`.
Events that contain a `t:logger:report_cb()` will have this printable string as its `message`.
""".

-behaviour(logger_handler).
-compile(
    {inline, [
        ensure_not_from_worker/1,
        ensure_structured_with_exception/1,
        ensure_mfa_and_level/2,
        make_msg_structured/2,
        apply_report_callback/2
    ]}
).

-export([log/2]).

-type logger_report() ::
    {io:format(), [term()]}
    | {report, logger:report()}
    | {string, unicode:chardata()}.

-doc "OTP logger-compliant handler for BugSnag".
-spec log(logger:log_event(), logger_handler:config()) -> any().
log(LogEvent, Config) ->
    _ = catch extract(LogEvent, Config).

-spec extract(logger:log_event(), logger_handler:config()) -> any().
extract(#{level := Level, msg := Msg0, meta := Meta0}, #{config := BugSnagConfig}) ->
    ensure_not_from_worker(Meta0),
    Report0 = ensure_structured_with_exception(Msg0),
    Meta = ensure_mfa_and_level(Meta0, Level),
    Report = make_msg_structured(Report0, Meta0),
    bugsnag:notify(BugSnagConfig, Report, Meta).

%% We cannot extract information from a non-structured report
-spec ensure_structured_with_exception(logger_report()) -> map() | no_return().
ensure_structured_with_exception({report, #{class := _, reason := _, stacktrace := _} = Report}) ->
    Report;
ensure_structured_with_exception({report, #{kind := _, reason := _, stacktrace := _} = Report}) ->
    Report;
ensure_structured_with_exception(
    {report, #{error := #{kind := _, message := _, stack := _}} = Report}
) ->
    Report;
ensure_structured_with_exception(_) ->
    throw(ignore_non_structured_logs).

%% Do not infinite-loop logs from the worker module
-spec ensure_not_from_worker(logger:metadata()) -> ok | no_return().
ensure_not_from_worker(#{mfa := {bugsnag_worker, _, _}}) ->
    throw(ignore_bugsnag_worker_logs);
ensure_not_from_worker(_) ->
    ok.

-spec ensure_mfa_and_level(logger:metadata(), logger:level()) -> bugsnag:metadata().
%% No mfa or line supplied in this one, we'll put in placeholders.
ensure_mfa_and_level(Meta, Level) when is_map(Meta), not is_map_key(mfa, Meta) ->
    Meta#{mfa => {undefined, undefined, 0}, line => 0, level => Level};
ensure_mfa_and_level(Meta, Level) ->
    Meta#{level => Level}.

-spec make_msg_structured(map(), logger:metadata()) -> map().
make_msg_structured(#{} = Report, #{report_cb := ReportCb}) when is_function(ReportCb) ->
    Report#{message => apply_report_callback(Report, ReportCb)};
make_msg_structured(#{} = Report, _Meta) ->
    Report.

%%% Applies a report callback function to a given report.
-spec apply_report_callback(map(), logger:report_cb()) -> binary().
apply_report_callback(Report, ReportCb) when is_function(ReportCb, 1) ->
    {Format, Terms} = ReportCb(Report),
    iolist_to_binary(io_lib:format(Format, Terms));
apply_report_callback(Report, ReportCb) when is_function(ReportCb, 2) ->
    DefaultConfig = #{depth => unlimited, chars_limit => 256, single_line => true},
    iolist_to_binary(ReportCb(Report, DefaultConfig)).
