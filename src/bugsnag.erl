-module(bugsnag).
-moduledoc """
BugSnag Erlang client.

## Configuration
It can be configured using application environment variables.
```erlang
{bugsnag_erlang, [
    {enabled, true},
    {api_key, "BUGSNAG_API_KEY"},
    {release_stage, "production"}
]}
```
If `enabled` is set to other than true, the rest of the configuration won't be read
and no handler will be registered. For the rest of the keys, see `t:config/0`.
""".

-export([start_link/2]).
-export([notify/5, notify/7]).

-doc """
Configuration options for the Bugsnag client.

It takes the following configuration options:

- `api_key`: the Bugsnag API key, mandatory to provide.
- `release_stage`: the release stage of the application, defaults to `production`
""".
-type config() :: #{
    api_key := binary(),
    release_stage := atom()
}.

-export_type([config/0]).

-doc """
Add a new global `bugsnag_logger_handler` handler.
""".
-spec start_link(string(), string()) -> gen_server:start_ret().
start_link(ApiKey, ReleaseStage) ->
    bugsnag_worker:start_link(#{
        api_key => list_to_binary(ApiKey),
        release_stage => list_to_existing_atom(ReleaseStage)
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
    gen_server:cast(bugsnag_worker, Payload).

generate_trace() ->
    logger:info(#{what => generating_trace}),
    try
        throw(bugsnag_gen_trace)
    catch
        _:_:StackTrace ->
            StackTrace
    end.
