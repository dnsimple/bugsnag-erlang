Bugsnag notifier for Erlang applications.

## Dependencies

Requires Lager.

The following applications must be started:

    lager, inets, crypto, ssl

## Usage

You may send custom errors directly:

```erlang
bugsnag:notify(error, fake, "Testing bugsnag with a manual error report", no_module, 0).
```

Or use the Erlang error logger:

```erlang
error_logger:error_msg("A sample error caught by the bugsnag error logger.").
```

Or cause an error with a full stack trace:

```erlang
bugsnag:test_error().
```

When embedding, make sure to set up the configuration elements in your sys.config (or other config file):

```erlang
[
  {bugsnag_erlang, [
    {error_logger, true},
    {api_key, "ENTER_API_KEY"},
    {release_state, "development"}
  ]}
].
```

And start the application:

```erlang
application:start(bugsnag)
```

Or add the application to your .app configuration.

### Lager handler

We also provide a [lager](https://github.com/basho/lager) to report anything
above a certain level (by default, `error`) to Bugsnag.

For example, simply add
```
{bugsnag_lager_handler, critical}
```
to your lager handler config.

## Thanks

Thank you to Ken Pratt: his library https://github.com/kenpratt/erlbrake provided a lot of code for this library.
