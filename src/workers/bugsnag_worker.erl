-module(bugsnag_worker).
-moduledoc false.

-include_lib("kernel/include/logger.hrl").

-define(NOTIFY_ENDPOINT, <<"https://notify.bugsnag.com">>).
-define(NOTIFIER_URL, <<"https://github.com/dnsimple/bugsnag-erlang">>).
-define(NOTIFIER_ACC_LIMIT, 1000).

-behaviour(gen_server).

-export([start_link/2, notify_worker/2]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(bugsnag_state, {
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
-opaque state() :: #bugsnag_state{}.

-type payload() :: {event, bugsnag_api_error_reporting:event()}.

-export_type([state/0, payload/0]).

-spec start_link(pos_integer(), bugsnag:config()) -> gen_server:start_ret().
start_link(N, Config) ->
    gen_server:start_link(?MODULE, {N, Config}, []).

-spec notify_worker(bugsnag:config(), payload()) -> ok.
notify_worker(#{name := Name, pool_size := PoolSize}, Payload) ->
    Int = 1 + erlang:phash2(self(), PoolSize),
    Pid = ets:lookup_element(bugsnag_registry, {Name, Int}, 2),
    gen_server:cast(Pid, Payload).

% Gen server hooks
-spec init({undefined | pos_integer(), bugsnag:config()}) -> {ok, state()}.
init({N, #{name := Name} = Config}) ->
    ets:insert(bugsnag_registry, {{Name, N}, self()}),
    do_init(Config).

-spec handle_cast({event, bugsnag_api_error_reporting:event()}, state()) ->
    {noreply, state()}.
handle_cast({event, Event}, #bugsnag_state{} = State) when is_map(Event) ->
    maybe_send_next(Event, State);
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, bad_request, state()}.
handle_call(_, _, State) ->
    {reply, bad_request, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({http, {Ref, {{_, 200, _}, _, _}}}, #bugsnag_state{pending = Ref} = State) ->
    send_pending(State#bugsnag_state{pending = undefined});
handle_info(
    {http, {Ref, {{_, Status, ReasonPhrase}, _, _}}}, #bugsnag_state{pending = Ref} = State
) ->
    ?LOG_WARNING(#{what => send_status_failed, http_status => Status, http_reason => ReasonPhrase}),
    send_pending(State#bugsnag_state{pending = undefined});
handle_info({http, {Ref, Unknown}}, #bugsnag_state{pending = Ref} = State) ->
    ?LOG_WARNING(#{what => send_status_failed, http_reason => Unknown}),
    send_pending(State#bugsnag_state{pending = undefined});
handle_info(_Info, State) ->
    {noreply, State}.

% Internal API

maybe_send_next(
    Event,
    #bugsnag_state{acc = Acc, acc_size = Limit, acc_limit = Limit, base_event = BaseEvent} = State
) ->
    {{value, ToDiscard}, Acc1} = queue:out(Acc),
    ?LOG_WARNING(#{what => bugsnag_discarding_event_overflow, event => ToDiscard}),
    MergedEvent = maps:merge(BaseEvent, Event),
    send_pending(State#bugsnag_state{acc = queue:in(MergedEvent, Acc1)});
maybe_send_next(
    Event, #bugsnag_state{acc = Acc, acc_size = AccSize, base_event = BaseEvent} = State
) ->
    MergedEvent = maps:merge(BaseEvent, Event),
    send_pending(State#bugsnag_state{acc = queue:in(MergedEvent, Acc), acc_size = AccSize + 1}).

send_pending(
    #bugsnag_state{
        pending = undefined, acc = Acc, acc_size = N, api_key = ApiKey, base_report = BaseReport
    } =
        State
) when is_integer(N), 0 < N ->
    Report = BaseReport#{events := queue:to_list(Acc)},
    Ref = deliver_payload(ApiKey, Report, State),
    {noreply, State#bugsnag_state{pending = Ref, acc = queue:new(), acc_size = 0}};
send_pending(#bugsnag_state{acc_size = 0} = State) ->
    {noreply, State};
send_pending(#bugsnag_state{} = State) ->
    {noreply, State}.

-spec do_init(bugsnag:config()) -> {ok, state()}.
do_init(#{api_key := ApiKey, release_stage := ReleaseStage, notifier_name := Name} = Config) ->
    process_flag(trap_exit, true),
    Base = build_base_event(ReleaseStage),
    Report = build_base_report(Name),
    {ok, #bugsnag_state{
        acc_limit = maps:get(events_limit, Config, ?NOTIFIER_ACC_LIMIT),
        api_key = ApiKey,
        release_stage = ReleaseStage,
        endpoint = maps:get(endpoint, Config, ?NOTIFY_ENDPOINT),
        base_report = Report,
        base_event = Base
    }}.

deliver_payload(ApiKey, Payload, #bugsnag_state{endpoint = Endpoint}) ->
    Headers = [
        {"Bugsnag-Api-Key", ApiKey},
        {"Bugsnag-Payload-Version", "5"}
    ],
    do_deliver_payload(Endpoint, Headers, Payload).

-spec get_hostname() -> binary().
get_hostname() ->
    list_to_binary(net_adm:localhost()).

-spec get_version() -> binary().
get_version() ->
    case application:get_key(bugsnag_erlang, vsn) of
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

-spec build_base_report(binary()) -> bugsnag_api_error_reporting:error_report().
build_base_report(Name) ->
    #{
        'payloadVersion' => 5,
        notifier => #{
            url => ?NOTIFIER_URL,
            name => Name,
            version => get_version()
        },
        events => []
    }.

do_deliver_payload(Endpoint, Headers, Payload) ->
    Request = {Endpoint, Headers, "application/json", json:encode(Payload)},
    HttpOptions = [{timeout, 5000}],
    Options = [{sync, false}, {receiver, self()}],
    {ok, RequestId} = httpc:request(post, Request, HttpOptions, Options),
    RequestId.
