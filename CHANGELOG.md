# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## main

## 3.0.0

- Introduced support for OTP Logger
- Remove support for lager.

## 3.0.0-rc4

- Fix a bug getting the right app version

## 3.0.0-rc3

- Fix an issue where `httpc` doesn't support aliases until OTP27.2
- Add custom notifier names to handlers
- Stabilise tests

## 3.0.0-rc2

- Extra exception formats following conventions by datadog and telemetry.
- Modify how enabled flag is read

## 3.0.0-rc1

Added:

Support for logger introduced, and lager and error_logger deprecated and scheduled to be removed on the next big release.

Fixed:

- Split bugsnag api from worker
- Add app-startup verification and preparation of configuration
- Add support for `logger` and multiple logger handlers.
- Add support for asynchonous HTTP requests together with bounded pooling

## 2.0.1

Fixed:

- Added customer encoder using OTP27 json

Changed:

- Requires markdownlint-cli & yamllint to pass in CI before test

## 2.0.0

Changed:

- Bumps to OTP/27
- Replaced "jsx" with "json"

Added:

- erlfmt
- CONTRIBUTING.md
- CHANGELOG.md
- release process to hex.pm

## 1.1.0

- N/A
