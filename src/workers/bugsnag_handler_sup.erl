-module(bugsnag_handler_sup).
-moduledoc false.
%% See `m:bugsnag_sup`.
%%
%% This module defines a top supervisor for the Bugsnag handler.
%% It uses a `rest_for_one` strategy, that is, the order matters.
%%
%% 1. A `m:bugsnag_registry` worker responsible for creating an ETS table (and then hibernating).
%% 2. A `m:bugsnag_worker_sup` supervising a static pool of `m:bugsnag_worker` workers.
%% 3. A `m:bugsnag_register` worker responsible for registering the logger handler and unregistering on termination.
%%
%% The supervision tree is as follows
%%
%%                       bugsnag_sup
%%                           |
%%                      (dynamically)
%%                           |
%%                  bugsnag_handler_sup
%%                 /         |          \
%%                /          |           \
%%               /           |            \
%%  bugsnag_registry  bugsnag_worker_sup  bugsnag_register
%%  (keeps ets table)        |            (adds and remove logger handler)
%%                          /|\
%%                         / | \
%%                    [bugsnag_worker]

-behaviour(supervisor).

-export([start_link/1, init/1]).

-spec start_link(logger_handler:config()) -> supervisor:startlink_ret().
start_link(#{config := #{name := Name}} = LoggerConfig) ->
    supervisor:start_link({local, Name}, ?MODULE, LoggerConfig).

-spec init(logger_handler:config()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(#{config := Config} = LoggerConfig) ->
    Strategy = #{strategy => rest_for_one, intensity => 20, period => 10},
    Children = [
        #{
            id => bugsnag_registry,
            start => {bugsnag_registry, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [bugsnag_registry]
        },
        #{
            id => bugsnag_worker_sup,
            start => {bugsnag_worker_sup, start_link, [Config]},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [bugsnag_worker_sup]
        },
        #{
            id => bugsnag_register,
            start => {bugsnag_register, start_link, [LoggerConfig]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [bugsnag_register]
        }
    ],
    {ok, {Strategy, Children}}.
