-module(bugsnag_logger_handler).
-moduledoc false.

-export([log/2]).

-doc "OTP logger-compliant handler for BugSnag".
-spec log(logger:log_event(), logger:handler_config()) -> term().
log(LogEvent, Config) ->
    catch do_log(LogEvent, Config).

%%% Helper function around `log/2'
-spec do_log(logger:log_event(), logger:handler_config()) -> term().
%% No mfa or line supplied in this one, we'll put in placeholders.
do_log(#{meta := Meta} = LogEvent, Config) when not is_map_key(mfa, Meta) ->
    NewMeta = Meta#{mfa => {undefined, undefined, 0}, line => 0},
    do_log(LogEvent#{meta => NewMeta}, Config);
%% Legacy report_cb logs, just apply callback
do_log(
    #{msg := {report, Report}, meta := #{report_cb := ReportCallback} = Meta} = LogEvent, Config
) ->
    Message = apply_report_callback(Report, ReportCallback),
    NewLogEvent = LogEvent#{
        meta => maps:without([report_cb], Meta),
        msg => {string, Message}
    },
    do_log(NewLogEvent, Config);
do_log(#{msg := {report, Report}} = LogEvent, Config) ->
    Message = io_lib:format("~p", [Report]),
    do_log(LogEvent#{msg => {string, Message}}, Config);
do_log(#{msg := {Format, FormatArgs}} = LogEvent, Config) when
    is_list(Format), is_list(FormatArgs)
->
    Message = io_lib:format(Format, FormatArgs),
    do_log(LogEvent#{msg => {string, Message}}, Config);
do_log(
    #{
        level := _,
        msg := {string, Message},
        meta := #{pid := _, time := _, mfa := {Module, _, _}, line := Line}
    },
    #{config := Config}
) ->
    Payload = #{
        type => undefined,
        reason => error,
        message => Message,
        module => Module,
        line => Line,
        trace => erlang:process_info(self(), current_stacktrace),
        request => undefined
    },
    bugsnag_worker:notify_worker(Config, Payload);
do_log(_LogEvent, _Config) ->
    ok.

%%% Applies a report callback function to a given report.
-spec apply_report_callback(term(), fun()) -> iodata().
apply_report_callback(Report, ReportCallback) ->
    case erlang:fun_info(ReportCallback, arity) of
        {arity, 1} ->
            {Format, Terms} = ReportCallback(Report),
            io_lib:format(Format, Terms);
        {arity, 2} ->
            ReportCallback(
                Report,
                #{
                    depth => unlimited,
                    chars_limit => unlimited,
                    single_line => false
                }
            )
    end.
