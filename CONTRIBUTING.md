# Contributing to bugsnag-erlang

## Testing

Submit unit tests for your changes. You can test your changes on your machine by [running the test suite](README.md#testing):

```shell
make test
```

When you submit a PR, tests will also be run on the continuous integration environment [via GitHub Actions](https://github.com/dnsimple/bugsnag-erlang/actions/workflows/ci.yml).

You should run this command before releasing.


## Formatting

If your editor doesn't automatically format Erlang code using [erlfmt](https://github.com/WhatsApp/erlfmt), run:

```shell
make format
```


## Releasing

The following instructions uses `$VERSION` as a placeholder, where `$VERSION` is a `MAJOR.MINOR.BUGFIX` release such as `1.2.0`.

1. Run the test suite and ensure all the tests pass.

1. Finalize the `## main` section in `CHANGELOG.md` assigning the version.

1. Commit and push the changes

    ```shell
    git commit -a -m "Release $VERSION"
    git push origin main
    ```

1. Wait for CI to complete.

1. Create a signed tag.

    ```shell
    git tag -a v$VERSION -s -m "Release $VERSION"
    git push origin --tags
    ```

1. GitHub actions will take it from there and release to <https://hex.pm/packages/bugsnag_erlang>
