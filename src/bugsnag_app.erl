-module(bugsnag_app).
-moduledoc false.

-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> supervisor:startlink_ret() | {error, no_api_key}.
start(_Type, _Args) ->
    case is_enabled() of
        true ->
            ?LOG_INFO(#{what => starting_bugsnag}),
            do_start();
        false ->
            %% we still need to start the sup to comply with the application behaviour
            ?LOG_INFO(#{what => bugsnag_disabled}),
            bugsnag_sup:start_link(disabled)
    end.

-spec stop(_) -> ok.
stop(_State) ->
    ?LOG_INFO(#{what => stopping_bugsnag}),
    ok.

-spec do_start() -> supervisor:startlink_ret() | {error, no_api_key}.
do_start() ->
    case get_api_key() of
        error ->
            {error, no_api_key};
        ApiKey ->
            maybe_set_error_logger(),
            Opts = #{
                api_key => ApiKey,
                release_stage => get_release_state(),
                name => get_handler_name(),
                pool_size => get_pool_size(),
                events_limit => get_events_limit()
            },
            bugsnag_sup:start_link(#{config => Opts})
    end.

-spec maybe_set_error_logger() -> any().
maybe_set_error_logger() ->
    IsErrorLoggerEnabled = true =:= application:get_env(bugsnag_erlang, error_logger, false),
    IsErrorLoggerEnabled andalso error_logger:add_report_handler(bugsnag_error_logger).

-spec is_enabled() -> boolean().
is_enabled() ->
    true =:= application:get_env(bugsnag_erlang, enabled, true).

-spec get_release_state() -> binary().
get_release_state() ->
    MaybeValue = application:get_env(bugsnag_erlang, release_state, production),
    case io_lib:latin1_char_list(MaybeValue) of
        true ->
            list_to_binary(MaybeValue);
        false ->
            case is_atom(MaybeValue) of
                true -> atom_to_binary(MaybeValue, utf8);
                false -> <<"production">>
            end
    end.

-spec get_api_key() -> binary() | error.
get_api_key() ->
    case application:get_env(bugsnag_erlang, api_key) of
        undefined ->
            error;
        {ok, "ENTER_API_KEY"} ->
            error;
        {ok, Value} when is_binary(Value) ->
            Value;
        {ok, Value} when is_list(Value) ->
            case io_lib:latin1_char_list(Value) of
                true -> list_to_binary(Value);
                false -> error
            end
    end.

-spec get_handler_name() -> atom().
get_handler_name() ->
    case application:get_env(bugsnag_erlang, handler_name) of
        undefined ->
            bugsnag_logger_handler;
        {ok, Value} when is_atom(Value) ->
            Value
    end.

-spec get_pool_size() -> pos_integer().
get_pool_size() ->
    case application:get_env(bugsnag_erlang, pool_size) of
        undefined ->
            erlang:system_info(schedulers);
        {ok, Value} when is_integer(Value), Value >= 1 ->
            Value
    end.

-spec get_events_limit() -> pos_integer().
get_events_limit() ->
    case application:get_env(bugsnag_erlang, events_limit) of
        undefined ->
            1000;
        {ok, Value} when is_integer(Value), Value >= 1 ->
            Value
    end.
