-module(bugsnag_sup).
-behavior(supervisor).

% API
-export([start_link/1]).

% Supervisor hooks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% Public API
start_link(Args) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, Args).

init(Args) ->
    {ok, {{one_for_one, 20, 10}, procs(Args)}}.

procs(disabled) ->
    %% bugsnag is disabled in the config
    [];
procs({ApiKey, ReleaseState}) ->
    [?CHILD(bugsnag, worker, [ApiKey, ReleaseState])].
