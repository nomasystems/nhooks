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

%%% EXTERNAL EXPORTS
-export([do/3, register_point/3]).

%%%% MACROS
-define(PERSISTENT_TERM_KEY(AppName, IntrospectionPoint),
    {nintrospection, AppName, IntrospectionPoint}
).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
do(AppName, IntrospectionPoint, Args) ->
    Points = persistent_term:get(?PERSISTENT_TERM_KEY(AppName, IntrospectionPoint), []),
    do(Points, Args).

register_point(AppName, IntrospectionPoint, Fun) when is_function(Fun) ->
    do_register_point(AppName, IntrospectionPoint, Fun);
register_point(AppName, IntrospectionPoint, {M, F} = Fun) when is_atom(M), is_atom(F) ->
    do_register_point(AppName, IntrospectionPoint, Fun);
register_point(_AppName, _IntrospectionPoint, Other) ->
    throw({badarg, Other}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
do([], _Args) ->
    ok;
do([{Mod, Fun} | T], Args) ->
    Mod:Fun(Args),
    do(T, Args);
do([Fun | T], Args) when is_function(Fun) ->
    Fun(Args),
    do(T, Args).

do_register_point(AppName, IntrospectionPoint, Fun) ->
    IntrospectionPoints = AppName:introspection_points(),
    case lists:member(IntrospectionPoint, IntrospectionPoints) of
        true ->
            ExistentFuns = persistent_term:get(
                ?PERSISTENT_TERM_KEY(AppName, IntrospectionPoint), []
            ),
            persistent_term:put(?PERSISTENT_TERM_KEY(AppName, IntrospectionPoint), [
                Fun | ExistentFuns
            ]);
        false ->
            erlang:error('non_existent_introspection_point')
    end.
