-module(bugsnag_register).
-moduledoc false.
%% See `m:bugsnag_sup`.

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2]).

-spec start_link(bugsnag:config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, [{hibernate_after, 0}]).

-spec init(logger_handler:config()) -> {ok, atom(), hibernate}.
init(#{config := #{name := Name}} = LoggerConfig) ->
    %% So that `terminate/2` is called
    process_flag(trap_exit, true),
    _ = logger:add_handler(Name, bugsnag_logger_handler, LoggerConfig),
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
