-module(bugsnag_logger_handler).
-moduledoc """
Logger handler module for BugSnag.

It is specially tailored to structured logging, and therefore unstructured logging will be treated
structurised ad-hoc, with the message being the only field of the `t:bugsnag:event/0`.
Events that contain a `t:logger:report_cb()` will have this printable string as its `message`.
""".

-behaviour(logger_handler).

-export([log/2]).

-doc "OTP logger-compliant handler for BugSnag".
-spec log(logger:log_event(), logger_handler:config()) -> any().
log(LogEvent, Config) ->
    catch extract(LogEvent, Config).

-spec extract(logger:log_event(), logger_handler:config()) -> any().
extract(#{level := Level, msg := Msg0, meta := Meta0}, #{config := BugSnagConfig}) ->
    Meta = ensure_mfa_and_level(Meta0, Level),
    Report = make_msg_structured(Msg0, Meta0),
    bugsnag:notify(BugSnagConfig, Report, Meta).

-spec ensure_mfa_and_level(logger:metadata(), logger:level()) -> bugsnag:metadata().
%% Do not infinite-loop logs from the worker module
ensure_mfa_and_level(#{mfa := {bugsnag_worker, _, _}}, _) ->
    throw(ignore_bugsnag_worker_logs);
%% No mfa or line supplied in this one, we'll put in placeholders.
ensure_mfa_and_level(Meta, Level) when is_map(Meta), not is_map_key(mfa, Meta) ->
    Meta#{mfa => {undefined, undefined, 0}, line => 0, level => Level};
ensure_mfa_and_level(Meta, Level) ->
    Meta#{level => Level}.

-spec make_msg_structured(
    {io:format(), [term()]}
    | {report, logger:report()}
    | {string, unicode:chardata()},
    logger:metadata()
) -> map().
make_msg_structured({report, #{} = Report}, #{report_cb := ReportCallback}) when
    is_function(ReportCallback)
->
    Report#{message => apply_report_callback(Report, ReportCallback)};
make_msg_structured({report, #{} = Report}, _Meta) ->
    Report;
make_msg_structured({report, Report}, _Meta) when is_list(Report) ->
    maps:from_list(Report);
make_msg_structured({string, Message}, _Meta) ->
    #{message => iolist_to_binary(Message)};
make_msg_structured({Format, Args}, _Meta) when is_list(Format), is_list(Args) ->
    #{message => iolist_to_binary(io_lib:format(Format, Args))}.

%%% Applies a report callback function to a given report.
-spec apply_report_callback(map(), logger:report_cb()) -> binary().
apply_report_callback(Report, ReportCallback) when is_function(ReportCallback, 1) ->
    {Format, Terms} = ReportCallback(Report),
    iolist_to_binary(io_lib:format(Format, Terms));
apply_report_callback(Report, ReportCallback) when is_function(ReportCallback, 2) ->
    DefaultConfig = #{depth => unlimited, chars_limit => 256, single_line => true},
    iolist_to_binary(ReportCallback(Report, DefaultConfig)).
