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
-module(nintrospection).

%%% INCLUDES
-include_lib("kernel/include/logger.hrl").

%%% EXTERNAL EXPORTS
-export([
    do/3,
    register_task/3,
    deregister_task/2,
    deregister_app_tasks/1,
    consult_tasks/2,
    consult_app_tasks/1
]).

%%%% MACROS
-define(PERSISTENT_TERM_KEY(AppName, IntrospectionPoint),
    {nintrospection, AppName, IntrospectionPoint}
).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
do(AppName, IntrospectionPoint, Args) ->
    Points = persistent_term:get(?PERSISTENT_TERM_KEY(AppName, IntrospectionPoint), []),
    do(AppName, IntrospectionPoint, Points, Args).

register_task(AppName, IntrospectionPoint, Task) when is_function(Task) ->
    do_register_task(AppName, IntrospectionPoint, Task);
register_task(AppName, IntrospectionPoint, {M, F} = Task) when is_atom(M), is_atom(F) ->
    do_register_task(AppName, IntrospectionPoint, Task);
register_task(_AppName, _IntrospectionPoint, Other) ->
    throw({badarg, Other}).

deregister_task(AppName, IntrospectionPoint) ->
    deregister_tasks(AppName, [IntrospectionPoint]).

deregister_app_tasks(AppName) ->
    IntrospectionPoints = AppName:introspection_points(),
    deregister_tasks(AppName, IntrospectionPoints).

consult_tasks(AppName, IntrospectionPoint) ->
    #{IntrospectionPoint := Tasks} = consult_tasks(AppName, [IntrospectionPoint], #{}),
    Tasks.

consult_app_tasks(AppName) ->
    IntrospectionPoints = AppName:introspection_points(),
    consult_tasks(AppName, IntrospectionPoints, #{}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
do(_AppName, _IntrospectionPoint, [], _Args) ->
    ok;
do(AppName, IntrospectionPoint, [Task | T], Args) ->
    try
        do_apply(Task, Args)
    catch
        Error:Reason ->
            log_error(AppName, IntrospectionPoint, Error, Reason)
    end,
    do(AppName, IntrospectionPoint, T, Args).

do_apply({Mod, Fun}, Args) ->
    erlang:apply(Mod, Fun, Args);
do_apply(Fun, Args) when is_function(Fun) ->
    erlang:apply(Fun, Args).

do_register_task(AppName, IntrospectionPoint, Task) ->
    IntrospectionPoints = AppName:introspection_points(),
    case lists:member(IntrospectionPoint, IntrospectionPoints) of
        true ->
            ExistentTasks = persistent_term:get(
                ?PERSISTENT_TERM_KEY(AppName, IntrospectionPoint), []
            ),
            persistent_term:put(?PERSISTENT_TERM_KEY(AppName, IntrospectionPoint), [
                Task | ExistentTasks
            ]);
        false ->
            erlang:error('non_existent_introspection_point')
    end.

deregister_tasks(_AppName, []) ->
    ok;
deregister_tasks(AppName, [IntrospectionPoint | Tl]) ->
    Key = ?PERSISTENT_TERM_KEY(AppName, IntrospectionPoint),
    persistent_term:erase(Key),
    deregister_tasks(AppName, Tl).

consult_tasks(_AppName, [], Acc) ->
    Acc;
consult_tasks(AppName, [IntrospectionPoint | Tl], Acc) ->
    Tasks = persistent_term:get(?PERSISTENT_TERM_KEY(AppName, IntrospectionPoint), []),
    consult_tasks(AppName, Tl, Acc#{IntrospectionPoint => Tasks}).

log_error(AppName, IntrospectionPoint, Error, Reason) ->
    ?LOG_ERROR(
        "[nintrospection] Instrospection point task failed. AppName: ~p. Point: ~p. Error: ~p:~p", [
            AppName, IntrospectionPoint, Error, Reason
        ]
    ).
