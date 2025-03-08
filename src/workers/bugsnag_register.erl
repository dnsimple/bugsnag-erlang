-module(bugsnag_register).
-moduledoc false.
%% See `m:bugsnag_sup`.

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2]).

-spec start_link(bugsnag:config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, [{hibernate_after, 0}]).

-spec init(bugsnag:config()) -> {ok, atom(), hibernate}.
init(#{name := Name} = Config) ->
    %% So that `terminate/2` is called
    process_flag(trap_exit, true),
    _ = logger:add_handler(Name, bugsnag_logger_handler, #{config => Config}),
    {ok, Name, hibernate}.

-spec handle_call(term(), gen_server:from(), atom()) -> {reply, ok, atom(), hibernate}.
handle_call(_Request, _From, State) ->
    {reply, ok, State, hibernate}.

-spec handle_cast(term(), atom()) -> {noreply, atom(), hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), atom()) -> ok.
terminate(_Reason, Name) ->
    ok = logger:remove_handler(Name).
