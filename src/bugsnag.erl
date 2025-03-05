-module(bugsnag).

-include_lib("kernel/include/logger.hrl").

-behavior(gen_server).

-export([start_link/2, notify/5, notify/7]).
-ignore_xref([start_link/2]).
-ifdef(TEST).
-export([test_error/0]).
-endif.

-type opts() :: {string(), string()}.

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    api_key :: string(),
    release_stage :: string()
}).
-type state() :: #state{}.

-type payload() ::
    {exception, atom(), atom() | string(), string() | binary(), atom(), non_neg_integer(), [term()], term()}
    | test_error.

-define(NOTIFY_ENDPOINT, "https://notify.bugsnag.com").
-define(NOTIFIER_NAME, <<"Bugsnag Erlang">>).
-define(NOTIFIER_VERSION, <<"2.0.1">>).
-define(NOTIFIER_URL, <<"https://github.com/dnsimple/bugsnag-erlang">>).

-spec start_link(string(), string()) -> gen_server:start_ret().
start_link(ApiKey, ReleaseStage) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {ApiKey, ReleaseStage}, []).

-spec notify(atom(), atom() | string(), string() | binary(), module(), non_neg_integer()) -> ok.
notify(Type, Reason, Message, Module, Line) ->
    notify(Type, Reason, Message, Module, Line, generate_trace(), undefined).

-spec notify(atom(), atom() | string(), string() | binary(), module(), non_neg_integer(), [term()], term()) -> ok.
notify(Type, Reason, Message, Module, Line, Trace, Request) ->
    gen_server:cast(?MODULE, {exception, Type, Reason, Message, Module, Line, Trace, Request}).

-ifdef(TEST).
-spec test_error() -> ok.
test_error() ->
    gen_server:cast(?MODULE, test_error).
-endif.

% Gen server hooks
-spec init(opts()) -> {ok, state()}.
init({ApiKey, ReleaseStage}) ->
    {ok, #state{api_key = ApiKey, release_stage = ReleaseStage}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast(payload(), state()) -> {noreply, state()}.
handle_cast({exception, Type, Reason, Message, Module, Line, Trace, Request}, State) ->
    send_exception(Type, Reason, Message, Module, Line, Trace, Request, State),
    {noreply, State};
handle_cast(test_error, State) ->
    erlang:error(test_error),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Message, State) ->
    {noreply, State}.

% Internal API

encoder([{_, _} | _] = Value, Encode) -> json:encode_key_value_list(Value, Encode);
encoder(Other, Encode) -> json:encode_value(Other, Encode).

custom_encode(Value) -> json:encode(Value, fun(Value, Encode) -> encoder(Value, Encode) end).

% See https://docs.bugsnag.com/api/error-reporting/#api-reference
send_exception(_Type, Reason, Message, _Module, _Line, Trace, _Request, State) ->
    Payload = [
        {apiKey, to_bin(State#state.api_key)},
        {payloadVersion, <<"5">>},
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
                    {releaseStage, to_bin(State#state.release_stage)}
                ]},
                {exceptions, [
                    [
                        {errorClass, to_bin(Reason)},
                        {message, to_bin(Message)},
                        {stacktrace, process_trace(Trace)}
                    ]
                ]}
            ]
        ]}
    ],
    deliver_payload(iolist_to_binary(custom_encode(Payload))).

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
                    {lineNumber, Line},
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

generate_trace() ->
    ?LOG_INFO(#{what => generating_trace}),
    try
        throw(bugsnag_gen_trace)
    catch
        _:_:StackTrace ->
            StackTrace
    end.
