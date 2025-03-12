-module(bugsnag_worker_sup).
-moduledoc false.
%% See `m:bugsnag_sup`.

-behaviour(supervisor).

-export([start_link/1, init/1]).

-spec start_link(bugsnag:config()) -> supervisor:startlink_ret().
start_link(Config) ->
    supervisor:start_link(?MODULE, Config).

-spec init(bugsnag:config()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{pool_size := PoolSize} = Config) ->
    Strategy = #{strategy => one_for_one, intensity => 5, period => 10},
    Children = [
        #{
            id => {bugsnag_worker, N},
            start => {bugsnag_worker, start_link, [N, Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [bugsnag_worker]
        }
     || N <- lists:seq(1, PoolSize)
    ],
    {ok, {Strategy, Children}}.
