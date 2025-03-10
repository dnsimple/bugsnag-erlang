-module(bugsnag_lager_handler).
-moduledoc "A `m:lager` handler to report messages above a certain level to BugSnag".
-deprecated({'_', '_', next_major_release}).

-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    handle_info/2
]).

-record(state, {
    level :: {mask, integer()}
}).
-doc "State of the lager handler `gen_event` worker".
-opaque state() :: #state{}.
-export_type([state/0]).

-define(DEFAULT_LEVEL, error).

-doc false.
-spec init(atom() | proplists:proplist()) -> {ok, state()}.
init(Level) when is_atom(Level) ->
    init([{level, Level}]);
init(Options) ->
    Level = proplists:get_value(level, Options, ?DEFAULT_LEVEL),
    {ok, #state{level = lager_util:config_to_mask(Level)}}.

-doc false.
-spec handle_call(term(), state()) -> {ok, term(), state()}.
handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) when is_atom(Level); is_list(Level) ->
    {ok, ok, State#state{level = lager_util:config_to_mask(Level)}};
handle_call(_Request, State) ->
    {ok, ok, State}.

-doc false.
-spec handle_event(term(), state()) -> {ok, state()}.
handle_event({log, Message}, #state{level = Level} = State) ->
    case lager_util:is_loggable(Message, Level, ?MODULE) of
        true ->
            ok = notify(Message),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

-doc false.
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) ->
    {ok, State}.

notify(Message) ->
    ok = bugsnag:notify(
        lager_msg,
        reason(Message),
        message(Message),
        module(Message),
        line(Message)
    ).

reason(Message) ->
    Severity = lager_msg:severity(Message),
    "lager:" ++ erlang:atom_to_list(Severity).

message(Message) ->
    lager_msg:message(Message).

module(Message) ->
    get_metadata(module, Message).

line(Message) ->
    get_metadata(line, Message).

get_metadata(Key, Message) ->
    Metadata = lager_msg:metadata(Message),
    case lists:keyfind(Key, 1, Metadata) of
        false -> undefined;
        {Key, Value} -> Value
    end.
