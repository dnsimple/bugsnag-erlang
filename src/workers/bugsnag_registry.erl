-module(bugsnag_registry).
-moduledoc false.
%% See `m:bugsnag_sup`.

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2]).

-spec start_link(bugsnag:config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, [{hibernate_after, 0}]).

-spec init(bugsnag:config()) -> {ok, no_state, hibernate}.
init(#{name := Name}) ->
    _ = ets:new(Name, [named_table, public, set, {read_concurrency, true}]),
    {ok, no_state, hibernate}.

-spec handle_call(term(), gen_server:from(), no_state) -> {reply, ok, no_state, hibernate}.
handle_call(_Request, _From, State) ->
    {reply, ok, State, hibernate}.

-spec handle_cast(term(), no_state) -> {noreply, no_state, hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.
