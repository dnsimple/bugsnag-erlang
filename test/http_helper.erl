-module(http_helper).

-export([start/2, stop/0, init/2, terminate/3]).
-behaviour(cowboy_handler).

start(Path, HandleFun) ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{Path, ?MODULE, HandleFun}]}]),
    {ok, _} = cowboy:start_clear(
        http_helper_listener,
        #{socket_opts => [{port, 0}], num_acceptors => 100, max_connections => infinity},
        #{env => #{dispatch => Dispatch}}
    ),
    ranch:get_port(http_helper_listener).

stop() ->
    cowboy:stop_listener(http_helper_listener).

%% Cowboy handler callbacks

init(Req, HandleFun) ->
    Req2 = HandleFun(Req),
    {ok, Req2, no_state}.

terminate(_Reason, _Req, _State) ->
    ok.
