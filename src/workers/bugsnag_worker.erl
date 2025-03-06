-module(bugsnag_worker).
-moduledoc false.

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, start_link/2, notify_worker/2]).

-deprecated([{start_link, 1, "To be removed when we remove support for lager and error_logger"}]).

-ifdef(TEST).
-export([deliver_payload/1, test_error/0]).
-endif.

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    api_key :: binary(),
    release_stage :: atom()
}).
-opaque state() :: #state{}.

-type payload() ::
    #{
        type := atom(),
        reason := atom() | string(),
        message := string() | binary(),
        module := module(),
        line := non_neg_integer(),
        trace := [term()],
        request := term()
    }
    | test_error.

-export_type([state/0, payload/0]).

-define(NOTIFY_ENDPOINT, "https://notify.bugsnag.com").
-define(NOTIFIER_NAME, <<"Bugsnag Erlang">>).
-define(NOTIFIER_VERSION, <<"2.0.1">>).
-define(NOTIFIER_URL, <<"https://github.com/dnsimple/bugsnag-erlang">>).

-spec start_link(bugsnag:config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {undefined, Config}, []).

-spec start_link(pos_integer(), bugsnag:config()) -> gen_server:start_ret().
start_link(N, Config) ->
    gen_server:start_link(?MODULE, {N, Config}, []).

-spec notify_worker(bugsnag:config(), payload()) -> ok.
notify_worker(#{name := Name, pool_size := PoolSize}, Payload) ->
    Int = 1 + erlang:phash2(self(), PoolSize),
    Pid = ets:lookup_element(Name, Int, 2),
    gen_server:cast(Pid, {payload, Payload}).

-ifdef(TEST).
-spec test_error() -> ok.
test_error() ->
    gen_server:cast(?MODULE, test_error).
-endif.

% Gen server hooks
-spec init({undefined | pos_integer(), bugsnag:config()}) -> {ok, state()}.
init({undefined, #{api_key := ApiKey, release_stage := ReleaseStage}}) ->
    process_flag(trap_exit, true),
    {ok, #state{api_key = ApiKey, release_stage = ReleaseStage}};
init({N, #{name := Name, api_key := ApiKey, release_stage := ReleaseStage}}) ->
    process_flag(trap_exit, true),
    ets:insert(Name, {N, self()}),
    {ok, #state{api_key = ApiKey, release_stage = ReleaseStage}}.

-spec handle_cast(payload(), state()) -> {noreply, state()}.
handle_cast(
    #{
        type := Type,
        reason := Reason,
        message := Message,
        module := Module,
        line := Line,
        trace := Trace,
        request := Request
    },
    State
) ->
    send_exception(Type, Reason, Message, Module, Line, Trace, Request, State),
    {noreply, State};
handle_cast(test_error, State) ->
    erlang:error(test_error),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Message, State) ->
    {noreply, State}.

% Internal API

% See https://docs.bugsnag.com/api/error-reporting/#api-reference
send_exception(_Type, Reason, Message, _Module, _Line, Trace, _Request, State) ->
    Payload = [
        {'apiKey', State#state.api_key},
        {'payloadVersion', <<"5">>},
        {notifier, [
            {name, ?NOTIFIER_NAME},
            {version, ?NOTIFIER_VERSION},
            {url, ?NOTIFIER_URL}
        ]},
        {events, [
            [
                {device, [
                    {hostname, to_bin(net_adm:localhost())}
                ]},
                {app, [
                    {'releaseStage', State#state.release_stage}
                ]},
                {exceptions, [
                    [
                        {'errorClass', to_bin(Reason)},
                        {message, to_bin(Message)},
                        {stacktrace, process_trace(Trace)}
                    ]
                ]}
            ]
        ]}
    ],
    deliver_payload(iolist_to_binary(custom_encode(Payload))).

encoder([{_, _} | _] = Value, Encode) ->
    json:encode_key_value_list(Value, Encode);
encoder(Other, Encode) ->
    json:encode_value(Other, Encode).

custom_encode(Value) ->
    json:encode(Value, fun(V, Encode) -> encoder(V, Encode) end).

process_trace(Trace) ->
    ?LOG_INFO(#{what => processing_trace, trace => Trace}),
    process_trace(Trace, []).

process_trace([], ProcessedTrace) ->
    ProcessedTrace;
process_trace([Current | Rest], ProcessedTrace) ->
    StackTraceLine =
        case Current of
            {_, F, _, [{file, File}, {line, Line}]} ->
                [
                    {file, to_bin(File)},
                    {'lineNumber', Line},
                    {method, to_bin(F)}
                ];
            {_, F, _} ->
                [
                    {method, to_bin(F)}
                ];
            _ ->
                ?LOG_WARNING(#{what => discarding_stack_trace_line, line => Current}),
                []
        end,
    process_trace(Rest, ProcessedTrace ++ [StackTraceLine]).

deliver_payload(Payload) ->
    ?LOG_INFO(#{what => sending_exception, exception => Payload}),
    case
        httpc:request(
            post, {?NOTIFY_ENDPOINT, [], "application/json", Payload}, [{timeout, 5000}], []
        )
    of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            ?LOG_INFO(#{what => received_response, response => Body});
        {_, {{_Version, Status, ReasonPhrase}, _Headers, _Body}} ->
            ?LOG_WARNING(#{what => send_status_failed, status => Status, reason => ReasonPhrase})
    end,

    ok.

to_bin(Atom) when erlang:is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
to_bin(Bin) when erlang:is_binary(Bin) ->
    Bin;
to_bin(Int) when erlang:is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_bin(List) when erlang:is_list(List) ->
    erlang:iolist_to_binary(List).
