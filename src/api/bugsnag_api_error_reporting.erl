-module(bugsnag_api_error_reporting).
-moduledoc false.

-type error_report() :: #{
    'apiKey' => binary(),
    'payloadVersion' := 5,
    notifier := notifier(),
    events := [event()]
}.

-type notifier() :: #{
    name := text(),
    version := text(),
    url := text(),
    dependencies => [dependency()]
}.

-type dependency() :: #{
    name := binary(),
    version := binary(),
    url := binary()
}.

-type severity() :: error | warning | info.

-type event() :: #{
    exceptions := [exception()],
    breadcrumbs => [breadcrumb()],
    request => request(),
    threads => [thread()],
    context => text(),
    groupingHash => binary(),
    unhandled => boolean(),
    severity => severity(),
    severityReason => severity_reason(),
    projectPackages => [binary()],
    user => user(),
    app => app(),
    device => device(),
    session => session(),
    featureFlags => [feature_flag()],
    metaData => map()
}.

-type exception() :: #{
    %% We'll use here Erlang exceptions' "class"
    'errorClass' := throw | error | exit,
    %% We'll use here Erlang exceptions' "reason"
    message => binary(),
    stacktrace := [stackframe()],
    type => binary()
}.

-type stackframe() :: #{
    file := binary(),
    'lineNumber' := integer(),
    'columnNumber' => integer(),
    method := binary(),
    inProject => boolean(),
    code => map(),
    codeIdentifier => binary()
}.

-type breadcrumb() :: #{
    timestamp := string() | binary(),
    name := text(),
    type := breadcrumb_type(),
    metaData => map()
}.

-type breadcrumb_type() ::
    navigation | request | process | log | user | state | error | manual.

-type request() :: #{
    clientIp => binary(),
    headers => map(),
    httpMethod => binary(),
    url => binary(),
    referer => binary()
}.

-type thread() :: #{
    id := binary(),
    name => binary(),
    errorReportingThread => boolean(),
    stacktrace := [stackframe()],
    state => binary(),
    type => binary()
}.

-type severity_reason() :: #{
    type => text(),
    attributes => map(),
    unhandledOverridden => boolean()
}.

-type user() :: #{
    id => binary(),
    name => binary(),
    email => binary()
}.

-type app() :: #{
    id => binary(),
    version => binary(),
    versionCode => integer(),
    bundleVersion => binary(),
    codeBundleId => binary(),
    buildUUID => binary(),
    releaseStage := binary(),
    type => binary(),
    dsymUUIDs => [binary()],
    duration => integer(),
    durationInForeground => integer(),
    inForeground => boolean(),
    isLaunching => boolean(),
    binaryArch => binary(),
    runningOnRosetta => boolean()
}.

-type device() :: #{
    hostname => binary(),
    id => binary(),
    manufacturer => binary(),
    model => binary(),
    modelNumber => binary(),
    osName => text(),
    osVersion => binary(),
    freeMemory => integer(),
    totalMemory => integer(),
    freeDisk => integer(),
    browserName => binary(),
    browserVersion => binary(),
    jailbroken => boolean(),
    orientation => binary(),
    time => binary(),
    cpuAbi => [binary()],
    runtimeVersions => map(),
    macCatalystiOSVersion => binary()
}.

-type session() :: #{
    id := binary(),
    startedAt := binary(),
    events := session_events()
}.

-type session_events() :: #{
    handled := integer(),
    unhandled := integer()
}.

-type feature_flag() :: #{
    featureFlag := binary(),
    variant => binary()
}.

-type text() :: atom() | string() | binary().

-export_type([
    error_report/0,
    event/0,
    stackframe/0,
    exception/0,
    breadcrumb/0,
    breadcrumb_type/0,
    text/0,
    severity/0,
    severity_reason/0
]).
