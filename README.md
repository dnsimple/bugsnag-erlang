# Insight Hub (Bugsnag) notifier for Erlang

[![Build Status](https://github.com/dnsimple/bugsnag-erlang/actions/workflows/ci.yml/badge.svg)](https://github.com/dnsimple/bugsnag-erlang/actions/workflows/ci.yml)
[![Module Version](https://img.shields.io/hexpm/v/bugsnag_erlang.svg)](https://hex.pm/packages/bugsnag_erlang)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/bugsnag_erlang/)
[![Hex Downloads](https://img.shields.io/hexpm/dt/bugsnag_erlang.svg?maxAge=2592000)](https://hex.pm/packages/bugsnag_erlang)
[![Coverage Status](https://coveralls.io/repos/github/dnsimple/bugsnag-erlang/badge.svg?branch=main)](https://coveralls.io/github/dnsimple/bugsnag-erlang?branch=main)

## Usage

You may send custom errors directly:

```erlang
bugsnag:notify(error, fake, "Testing bugsnag with a manual error report", no_module, 0).
```

Or use the Erlang error logger:

```erlang
error_logger:error_msg("A sample error caught by the bugsnag error logger.").
```

Or `logger`:

```erlang
?LOG_ERROR(#{what => example_error, text => "A sample error caught by the bugsnag logger handler"}).
```

When embedding, make sure to set up the configuration elements in your `sys.config` (or other config file),
See `m:bugsnag` for configuration details.

## Lager handler

We also provide a [lager](https://github.com/basho/lager) handler to report anything
above a certain level (by default, `error`) to Bugsnag.

For example, simply add to your `sys.config`

```erlang
{bugsnag_lager_handler, critical}
```

to your lager handler config.

## Testing

To test the codebase:

```shell
make test
```

## Formatting

To format the codebase

```shell
make format
```
