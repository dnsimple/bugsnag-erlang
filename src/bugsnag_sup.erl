-module(bugsnag_sup).
-moduledoc false.
%% This is the top supervisor of the application, and there'll be only one of them.
%% When adding handlers, we will add a new `m:bugsnag_handler_sup`

-behaviour(supervisor).

-export([start_link/1, init/1, add_handler/1, remove_handler/1]).

-type config() :: disabled | logger_handler:config().
-export_type([config/0]).

-spec start_link(config()) -> supervisor:startlink_ret().
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

-spec add_handler(logger_handler:config()) -> supervisor:startchild_ret().
add_handler(LoggerConfig) ->
    supervisor:start_child(?MODULE, proc(LoggerConfig)).

-spec remove_handler(logger_handler:id()) -> ok | {error, term()}.
remove_handler(Name) ->
    supervisor:terminate_child(?MODULE, Name).

-spec init(config()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Args) ->
    Strategy = #{strategy => one_for_one, intensity => 120, period => 5},
    Children = procs(Args),
    {ok, {Strategy, Children}}.

-spec procs(config()) -> [supervisor:child_spec()].
procs(disabled) ->
    %% bugsnag is disabled in the config
    [];
procs(Config) ->
    [proc(Config)].

proc(#{config := #{name := Name}} = LoggerConfig) ->
    #{
        id => Name,
        start => {bugsnag_handler_sup, start_link, [LoggerConfig]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [bugsnag_handler_sup]
    }.
