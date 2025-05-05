-module(bugsnag_registry).
-moduledoc false.
%% See `m:bugsnag_sup`.

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link(?MODULE, noargs, [{hibernate_after, 0}]).

-spec init(noargs) -> {ok, no_state, hibernate}.
init(noargs) ->
    _ = ets:new(?MODULE, [named_table, public, set, {read_concurrency, true}]),
    {ok, no_state, hibernate}.

-spec handle_call(term(), gen_server:from(), no_state) -> {reply, ok, no_state, hibernate}.
handle_call(_Request, _From, State) ->
    {reply, ok, State, hibernate}.

-spec handle_cast(term(), no_state) -> {noreply, no_state, hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.
