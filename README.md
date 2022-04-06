# nhooks

Erlang hooks application. It allows adding tasks dynamically to predefined hooks.

## Status

![nthrottle](https://github.com/nomasystems/nhooks/actions/workflows/build.yml/badge.svg)

## Prerequisites

![Min. OTP version](https://img.shields.io/badge/min._OTP-22-blue)
![Max. OTP version](https://img.shields.io/badge/max._OTP-24-blue)
![Min. rebar version](https://img.shields.io/badge/min._rebar-3.14.X-blue)

## Configuration

In your `rebar.config` file, add the dependency:
```erl
{deps, [
    {nhooks, {git, "git@github.com:nomasystems/nhooks.git", {branch, "main"}}}
]}.
```

## Usage example:

Assuming there is an app named test_app that has one hook named init and another named terminate:

```erl
-module(test_app).

%%% EXTERNAL EXPORTS
-export([init/1, terminate/1]).

%%% NHOOKS EXPORTS
-export([hooks/0]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
init(Term) ->
    nhooks:do(test_app, init, [Term]),
    ok.

terminate(Term) ->
    nhooks:do(test_app, terminate, [Term]),
    ok.

hooks() ->
    [
        init,
        terminate
    ].


```

You can register tasks to the defined hooks:

```erl

%%% Registering tasks in both hooks
ok = nhooks:register_task(test_app, init, fun(Term) ->
    do_something(Term)
end).

ok = nhooks:register_task(test_app, terminate, fun(Term) ->
    do_something_else(Term)
end).

```

From now on when test_app executes the init and terminate hooks the registered tasks will be executed.

## Support

Any doubt or suggestion? Please, read the [documentation](http://nomasystems.github.io/nhooks) and check out [our issue tracker](https://github.com/nomasystems/nhooks/issues).
