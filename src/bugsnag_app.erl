-module(bugsnag_app).

-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> supervisor:startlink_ret().
start(_Type, _Args) ->
    case application:get_env(bugsnag_erlang, enabled, true) of
        true ->
            start();
        false ->
            ?LOG_INFO(#{what => bugsnag_disabled}),
            %% we still need to start the sup to comply with the application behaviour
            bugsnag_sup:start_link(disabled)
    end.

-spec start() -> supervisor:startlink_ret().
start() ->
    ?LOG_INFO(#{what => starting_bugsnag}),
    ReleaseState =
        case application:get_env(bugsnag_erlang, release_state) of
            {ok, Value} -> Value;
            undefined -> undefined
        end,
    case application:get_env(bugsnag_erlang, api_key) of
        {ok, "ENTER_API_KEY"} ->
            {error, no_api_key};
        {ok, ApiKey} ->
            case application:get_env(bugsnag_erlang, error_logger) of
                {ok, true} ->
                    error_logger:add_report_handler(bugsnag_error_logger);
                _ ->
                    ok
            end,
            Opts = #{
                api_key => list_to_binary(ApiKey),
                release_stage => list_to_binary(ReleaseState)
            },
            bugsnag_sup:start_link(Opts);
        undefined ->
            {error, no_api_key}
    end.

-spec stop(_) -> ok.
stop(_State) ->
    ?LOG_INFO(#{what => stopping_bugsnag}),
    ok.
