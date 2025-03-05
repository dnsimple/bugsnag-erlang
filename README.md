# Insight Hub (Bugsnag) notifier for Erlang

[![Build Status](https://github.com/dnsimple/bugsnag-erlang/actions/workflows/ci.yml/badge.svg)](https://github.com/dnsimple/bugsnag-erlang/actions/workflows/ci.yml)
[![Module Version](https://img.shields.io/hexpm/v/bugsnag_erlang.svg)](https://hex.pm/packages/bugsnag_erlang)

## Dependencies

Requires [Lager](https://github.com/erlang-lager/lager)

The following applications must be started

```text
kernel,stdlib,inets,crypto,ssl,lager
```

## Usage

You may send custom errors directly

```erlang
bugsnag:notify(error, fake, "Testing bugsnag with a manual error report", no_module, 0).
```

Or use the Erlang error logger

```erlang
error_logger:error_msg("A sample error caught by the bugsnag error logger.").
```

Or cause an error with a full stack trace

```erlang
bugsnag:test_error().
```

When embedding, make sure to set up the configuration elements in your sys.config (or other config file)

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

```erlang
{bugsnag_lager_handler, critical}
```

to your lager handler config.

## Formatting

To format the codebase

```shell
make format
```

## Thanks

Thank you to Ken Pratt: his library <https://github.com/kenpratt/erlbrake> provided a lot of code for this library.
