-module(bugsnag_worker).
-moduledoc false.

-include_lib("kernel/include/logger.hrl").

-define(NOTIFY_ENDPOINT, <<"https://notify.bugsnag.com">>).
-define(NOTIFIER_NAME, <<"Bugsnag Erlang">>).
-define(NOTIFIER_URL, <<"https://github.com/dnsimple/bugsnag-erlang">>).
-define(NOTIFIER_ACC_LIMIT, 1000).

-behaviour(gen_server).

-export([start_link/1, start_link/2, notify_worker/2]).

-deprecated([{start_link, 1, next_major_release}]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    pending :: undefined | reference(),
    acc = queue:new() :: queue:queue(map()),
    acc_size = 0 :: non_neg_integer(),
    acc_limit :: pos_integer(),
    api_key :: binary(),
    release_stage :: binary(),
    endpoint :: binary(),
    base_event :: bugsnag_api_error_reporting:event(),
    base_report :: bugsnag_api_error_reporting:error_report()
}).
-opaque state() :: #state{}.

-type legacy() ::
    #{
        type := atom(),
        reason := atom() | string(),
        message => string() | binary(),
        report => map(),
        module := module(),
        line := non_neg_integer(),
        trace := [term()],
        request := term()
    }.

-type payload() :: {legacy, legacy()} | {event, bugsnag_api_error_reporting:event()}.

-export_type([state/0, payload/0]).

-spec start_link(bugsnag:config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {undefined, Config}, [{debug, [log]}]).

-spec start_link(pos_integer(), bugsnag:config()) -> gen_server:start_ret().
start_link(N, Config) ->
    gen_server:start_link(?MODULE, {N, Config}, [{debug, [log]}]).

-spec notify_worker(bugsnag:config(), payload()) -> ok.
notify_worker(#{name := Name, pool_size := PoolSize}, Payload) ->
    Int = 1 + erlang:phash2(self(), PoolSize),
    Pid = ets:lookup_element(Name, Int, 2),
    gen_server:cast(Pid, Payload).

% Gen server hooks
-spec init({undefined | pos_integer(), bugsnag:config()}) -> {ok, state()}.
init({undefined, Config}) ->
    do_init(Config);
init({N, #{name := Name} = Config}) ->
    ets:insert(Name, {N, self()}),
    do_init(Config).

-spec handle_cast({legacy, payload()} | {event, bugsnag_api_error_reporting:event()}, state()) ->
    {noreply, state()}.
handle_cast({event, Event}, #state{} = State) when is_map(Event) ->
    send_pending(Event, State);
%% TODO: This is to support lager and error_logger, to be removed
handle_cast({legacy, Legacy}, #state{} = State) ->
    #{type := Type, reason := Reason, message := Message, trace := Trace} = Legacy,
    Event = #{
        exceptions => [
            #{
                'errorClass' => error_class(Reason),
                type => Type,
                message => to_bin(Message),
                stacktrace => process_trace(Trace)
            }
        ]
    },
    send_pending(Event, State);
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, bad_request, state()}.
handle_call(_, _, State) ->
    {reply, bad_request, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({http, {Ref, {{_, 200, _}, _, _}}}, #state{pending = Ref} = State) ->
    send_pending(State#state{pending = undefined});
handle_info({http, {Ref, {{_, Status, ReasonPhrase}, _, _}}}, #state{pending = Ref} = State) ->
    ?LOG_WARNING(#{what => send_status_failed, status => Status, reason => ReasonPhrase}),
    send_pending(State#state{pending = undefined});
handle_info({http, {Ref, Unknown}}, #state{pending = Ref} = State) ->
    ?LOG_WARNING(#{what => send_status_failed, reason => Unknown}),
    send_pending(State#state{pending = undefined});
handle_info(_Info, State) ->
    {noreply, State}.

% Internal API

send_pending(
    Event, #state{acc = Acc, acc_size = Limit, acc_limit = Limit, base_event = BaseEvent} = State
) ->
    {{value, ToDiscard}, Acc1} = queue:out(Acc),
    ?LOG_WARNING(#{what => bugsnag_discarding_event_overflow, event => ToDiscard}),
    send_pending(State#state{acc = queue:in(maps:merge(BaseEvent, Event), Acc1)});
send_pending(Event, #state{acc = Acc, acc_size = AccSize, base_event = BaseEvent} = State) ->
    MergedEvent = maps:merge(BaseEvent, Event),
    send_pending(State#state{acc = queue:in(MergedEvent, Acc), acc_size = AccSize + 1}).

send_pending(
    #state{pending = undefined, acc = Acc, acc_size = N, api_key = ApiKey, base_report = BaseReport} =
        State
) when is_integer(N), 0 < N ->
    Report = BaseReport#{events := queue:to_list(Acc)},
    Ref = deliver_payload(ApiKey, Report, State),
    {noreply, State#state{pending = Ref, acc = queue:new()}};
send_pending(#state{acc_size = 0} = State) ->
    {noreply, State};
send_pending(#state{} = State) ->
    {noreply, State}.

-spec do_init(bugsnag:config()) -> {ok, state()}.
do_init(#{api_key := ApiKey, release_stage := ReleaseStage} = Config) ->
    process_flag(trap_exit, true),
    Base = build_base_event(ReleaseStage),
    Report = build_base_report(),
    {ok, #state{
        acc_limit = maps:get(events_limit, Config, ?NOTIFIER_ACC_LIMIT),
        api_key = ApiKey,
        release_stage = ReleaseStage,
        endpoint = maps:get(endpoint, Config, ?NOTIFY_ENDPOINT),
        base_report = Report,
        base_event = Base
    }}.

process_trace(StackTrace) ->
    process_trace(StackTrace, []).

process_trace([], ProcessedTrace) ->
    lists:reverse(ProcessedTrace);
process_trace([{Mod, Fun, Args, Info} | Rest], ProcessedTrace) when is_list(Args) ->
    Arity = length(Args),
    process_trace([{Mod, Fun, Arity, Info} | Rest], ProcessedTrace);
process_trace([{Mod, Fun, Arity, Info} | Rest], ProcessedTrace) when is_integer(Arity) ->
    LineNum = proplists:get_value(line, Info, 0),
    FunName = unicode:characters_to_binary(io_lib:format("~p:~p/~p", [Mod, Fun, Arity])),
    File = iolist_to_binary(proplists:get_value(file, Info, "")),
    Trace = #{
        file => File,
        'lineNumber' => LineNum,
        method => FunName
    },
    process_trace(Rest, [Trace | ProcessedTrace]);
process_trace([Current | Rest], ProcessedTrace) ->
    ?LOG_WARNING(#{what => discarding_stack_trace_line, line => Current}),
    process_trace(Rest, ProcessedTrace).

deliver_payload(ApiKey, Payload, #state{endpoint = Endpoint}) ->
    Headers = [
        {"Bugsnag-Api-Key", ApiKey},
        {"Bugsnag-Payload-Version", "5"}
    ],
    do_deliver_payload(Endpoint, Headers, Payload).

-spec error_class(term()) -> throw | error | exit.
error_class(throw) -> throw;
error_class(error) -> error;
error_class(exit) -> exit;
error_class(_) -> error.

to_bin(Atom) when erlang:is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
to_bin(Bin) when erlang:is_binary(Bin) ->
    Bin;
to_bin(Int) when erlang:is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_bin(List) when erlang:is_list(List) ->
    erlang:iolist_to_binary(List).

-spec get_hostname() -> binary().
get_hostname() ->
    list_to_binary(net_adm:localhost()).

-spec get_version() -> binary().
get_version() ->
    case application:get_key(bugsnag, vsn) of
        {ok, Vsn} when is_list(Vsn) ->
            list_to_binary(Vsn);
        _ ->
            <<"0.0.0">>
    end.

-spec build_base_event(binary()) -> bugsnag_api_error_reporting:event().
build_base_event(ReleaseStage) ->
    {_, OsName} = os:type(),
    #{
        device => #{hostname => get_hostname(), 'osName' => OsName},
        app => #{'releaseStage' => ReleaseStage},
        exceptions => []
    }.

-spec build_base_report() -> bugsnag_api_error_reporting:error_report().
build_base_report() ->
    #{
        'payloadVersion' => 5,
        notifier => #{
            url => ?NOTIFIER_URL,
            name => ?NOTIFIER_NAME,
            version => get_version()
        },
        events => []
    }.

do_deliver_payload(Endpoint, Headers, Payload) ->
    Request = {Endpoint, Headers, "application/json", json:encode(Payload)},
    Alias = alias([reply]),
    HttpOptions = [{timeout, 5000}],
    Options = [{sync, false}, {receiver, Alias}],
    {ok, RequestId} = httpc:request(post, Request, HttpOptions, Options),
    RequestId.
