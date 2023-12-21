%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License
-module(nhooks).

%%% INCLUDES
-include_lib("kernel/include/logger.hrl").

%%% EXTERNAL EXPORTS
-export([
    do/3,
    register_task/3,
    register_task/4,
    deregister_task/2,
    deregister_app_tasks/1,
    consult_tasks/2,
    consult_app_tasks/1
]).

%%%% MACROS
-define(DEFAULT_PRIORITY, 0).
-define(PERSISTENT_TERM_KEY(AppName, Hook),
    {nhooks, AppName, Hook}
).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
do(AppName, Hook, Args) ->
    Hooks = persistent_term:get(?PERSISTENT_TERM_KEY(AppName, Hook), []),
    do(AppName, Hook, Hooks, Args).

register_task(AppName, Hook, Task) ->
    register_task(AppName, Hook, Task, ?DEFAULT_PRIORITY).

register_task(AppName, Hook, Task, Priority) when is_function(Task) ->
    do_register_task(AppName, Hook, Task, Priority);
register_task(AppName, Hook, {M, F} = Task, Priority) when is_atom(M), is_atom(F) ->
    do_register_task(AppName, Hook, Task, Priority);
register_task(_AppName, _Hook, Other, _Priority) ->
    throw({badarg, Other}).

deregister_task(AppName, Hook) ->
    deregister_tasks(AppName, [Hook]).

deregister_app_tasks(AppName) ->
    Hooks = AppName:hooks(),
    deregister_tasks(AppName, Hooks).

consult_tasks(AppName, Hook) ->
    #{Hook := Tasks} = consult_tasks(AppName, [Hook], #{}),
    Tasks.

consult_app_tasks(AppName) ->
    Hooks = AppName:hooks(),
    consult_tasks(AppName, Hooks, #{}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
do(_AppName, _Hook, [], _Args) ->
    ok;
do(AppName, Hook, [{_Priority, Task} | T], Args) ->
    try
        case do_apply(Task, Args) of
            stop -> stopped;
            {stop, Data} -> {stopped, Data};
            _ -> do(AppName, Hook, T, Args)
        end
    catch
        Error:Reason ->
            log_error(AppName, Hook, Error, Reason),
            do(AppName, Hook, T, Args)
    end.

do_apply({Mod, Fun}, Args) ->
    erlang:apply(Mod, Fun, Args);
do_apply(Fun, Args) when is_function(Fun) ->
    erlang:apply(Fun, Args).

do_register_task(AppName, Hook, Task, Priority) ->
    Hooks = AppName:hooks(),
    case lists:member(Hook, Hooks) of
        true ->
            Key = ?PERSISTENT_TERM_KEY(AppName, Hook),
            ExistentTasks = persistent_term:get(Key, []),
            NewTask = {Priority, Task},
            SortedTasks = lists:merge(fun({X, _}, {Y, _}) -> X =< Y end, ExistentTasks, [NewTask]),
            persistent_term:put(Key, SortedTasks);
        false ->
            erlang:error('non_existent_hook')
    end.

deregister_tasks(_AppName, []) ->
    ok;
deregister_tasks(AppName, [Hook | Tl]) ->
    Key = ?PERSISTENT_TERM_KEY(AppName, Hook),
    persistent_term:erase(Key),
    deregister_tasks(AppName, Tl).

consult_tasks(_AppName, [], Acc) ->
    Acc;
consult_tasks(AppName, [Hook | Tl], Acc) ->
    Tasks = persistent_term:get(?PERSISTENT_TERM_KEY(AppName, Hook), []),
    consult_tasks(AppName, Tl, Acc#{Hook => Tasks}).

log_error(AppName, Hook, Error, Reason) ->
    ?LOG_ERROR(
        "[nhooks] Hook task failed. AppName: ~p. Hook: ~p. Error: ~p:~p", [
            AppName, Hook, Error, Reason
        ]
    ).
