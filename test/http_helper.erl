-module(http_helper).

-export([start/3, stop/1, init/2, terminate/3]).
-behaviour(cowboy_handler).

start(Name, Path, HandleFun) ->
    {ok, _} = application:ensure_all_started([cowboy, ranch]),
    Dispatch = cowboy_router:compile([{'_', [{Path, ?MODULE, HandleFun}]}]),
    {ok, _} = cowboy:start_clear(
        Name,
        #{socket_opts => [{port, 0}], num_acceptors => 100, max_connections => infinity},
        #{env => #{dispatch => Dispatch}}
    ),
    ranch:get_port(Name).

stop(Name) ->
    ok = cowboy:stop_listener(Name).

%% Cowboy handler callbacks

init(Req, HandleFun) ->
    Req2 = HandleFun(Req),
    {ok, Req2, no_state}.

terminate(_Reason, _Req, _State) ->
    ok.
