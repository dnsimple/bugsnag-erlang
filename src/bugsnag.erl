-module(bugsnag).
-moduledoc """
BugSnag Erlang client.

## Configuration
It can be configured using application environment variables or by passing a map to the
`start_link/1` function.

If using application environment variables, the config looks like the following:
```erlang
{bugsnag_erlang, [
    {enabled, true},
    {api_key, "BUGSNAG_API_KEY"},
    {release_stage, "production"},
    {handler_name, bugsnag_logger_handler},
    {pool_size, 10}
]}
```
If `enabled` is set to other than true, the rest of the configuration won't be read
and no handler will be registered. For the rest of the keys, see `t:config/0`.

If a new handler wants to be added, the `handler_name` key can be set to a new atom.
""".

-export([start_link/2, add_handler/1, remove_handler/1]).
-export([notify/5, notify/7]).
-deprecated([{start_link, 2, next_major_release}]).
-deprecated([{notify, 5, next_major_release}]).
-deprecated([{notify, 7, next_major_release}]).

-doc """
Configuration options for the Bugsnag client.

It takes the following configuration options:

- `api_key`: the Bugsnag API key, mandatory to provide.
- `release_stage`: the release stage of the application, defaults to `production`
- `name`: defaults to `bugsnag_logger_handler`, but allows to create more `logger` handlers
    with different configurations.
- `pool_size`: defaults to the number of schedulers, increases the number of workers in the pool
    in case of high load.
""".
-type config() :: #{
    api_key := binary(),
    release_stage := binary(),
    name := logger_handler:id(),
    pool_size := pos_integer()
}.

-export_type([config/0]).

-doc "Add a new logger handler.".
-spec add_handler(config()) -> supervisor:startchild_ret().
add_handler(Config) ->
    bugsnag_sup:add_handler(Config).

-doc "Remove a new logger handler.".
-spec remove_handler(config()) -> ok | {error, term()}.
remove_handler(#{name := Name}) ->
    bugsnag_sup:remove_handler(Name);
remove_handler(Name) ->
    bugsnag_sup:remove_handler(Name).

-doc """
Add a new global `bugsnag_logger_handler` handler.

This is deprecated, `add_handler/1` is preferred.
""".
-spec start_link(binary(), binary()) -> gen_server:start_ret().
start_link(ApiKey, ReleaseStage) ->
    bugsnag_worker:start_link(#{
        name => bugsnag_logger_handler,
        pool_size => 1,
        api_key => ApiKey,
        release_stage => ReleaseStage
    }).

-doc "Notify a global worker about an exception.".
-spec notify(atom(), atom() | string(), string() | binary(), module(), non_neg_integer()) -> ok.
notify(Type, Reason, Message, Module, Line) ->
    notify(Type, Reason, Message, Module, Line, generate_trace(), undefined).

-doc "Notify a global worker about an exception.".
-spec notify(
    atom(), atom() | string(), string() | binary(), module(), non_neg_integer(), [term()], term()
) -> ok.
notify(Type, Reason, Message, Module, Line, Trace, Request) ->
    Payload = #{
        type => Type,
        reason => Reason,
        message => Message,
        module => Module,
        line => Line,
        trace => Trace,
        request => Request
    },
    gen_server:cast(bugsnag_worker, {legacy, Payload}).

-spec generate_trace() -> list().
generate_trace() ->
    StepsBack = 1,
    case erlang:process_info(self(), current_stacktrace) of
        {_, StackTrace} when is_list(StackTrace) ->
            lists:nthtail(StepsBack, StackTrace);
        _ ->
            try
                throw(bugsnag_gen_trace)
            catch
                _:_:StackTrace ->
                    lists:nthtail(StepsBack, StackTrace)
            end
    end.
