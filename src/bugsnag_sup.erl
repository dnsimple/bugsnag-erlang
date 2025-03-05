-module(bugsnag_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-type opts() :: disabled | {string(), string()}.
-export_type([opts/0]).

-spec start_link(opts()) -> supervisor:startlink_ret().
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

-spec init(opts()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Args) ->
    Strategy = #{strategy => one_for_one, intensity => 20, period => 10},
    Children = procs(Args),
    {ok, {Strategy, Children}}.

-spec procs(opts()) -> [supervisor:child_spec()].
procs(disabled) ->
    %% bugsnag is disabled in the config
    [];
procs({ApiKey, ReleaseState}) ->
    Child = #{
        id => bugsnag,
        start => {bugsnag, start_link, [ApiKey, ReleaseState]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [bugsnag]
    },
    [Child].
