# nhooks

Erlang hooks application. It allows adding tasks dynamically to predefined hooks.

## Status

![nthrottle](https://github.com/nomasystems/nhooks/actions/workflows/build.yml/badge.svg)

## Prerequisites

![Min. OTP version](https://img.shields.io/badge/min._OTP-24-blue)
![Max. OTP version](https://img.shields.io/badge/max._OTP-25-blue)
![Min. rebar version](https://img.shields.io/badge/min._rebar-3.14.X-blue)

## Configuration

In your `rebar.config` file, add the dependency:
```erl
{deps, [
    {nhooks, {git, "git@github.com:nomasystems/nhooks.git", {branch, "main"}}}
]}.
```

## Usage example

Assuming there is a module named `test_app` that has one hook named `init` and another named `terminate`:

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
    nhooks:do(?MODULE, init, [Term]).

terminate(Term) ->
    nhooks:do(?MODULE, terminate, [Term]).

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

From now on when `test_app` executes the `init` and `terminate` hooks the registered tasks will be executed.


You can also register multiple tasks to any defined hook:

```erl

%%% Registering multiple tasks to same hook
ok = nhooks:register_task(test_app, init, fun(Term) ->
    do_something(Term)
end).

ok = nhooks:register_task(test_app, init, fun(Term) ->
    do_something_else(Term)
end).

```

And all will be executed when `test_app` executes the `init` hook.

If you want to stop the subsequent tasks from executing, you can return `stop` or `{stop, Data}` from any task. For example:

```erl

%%% Stopping subsequent tasks in the same hook
ok = nhooks:register_task(test_app, init, fun(Term) ->
    do_something(Term),
    {stop, {some, data}}
end).

ok = nhooks:register_task(test_app, init, fun(Term) ->
    do_something_else(Term)
end).

```

In this case, the second task won't be executed and the call to `nhooks:do/3` will return `{stopped, {some, data}}`.

All tasks are registered with a default priority of `0` and then executed from lowest to highest priority. That priority can be passed as the last argument of `nhooks:registar_task/4`:

```erl

ok = nhooks:register_task(test_app, init, fun(Term) -> first, -1).
ok = nhooks:register_task(test_app, init, fun(Term) -> second end).
ok = nhooks:register_task(test_app, init, fun(Term) -> third end, 5).

```


## Support

Any doubt or suggestion? Please check out [our issue tracker](https://github.com/nomasystems/nhooks/issues).
