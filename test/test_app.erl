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
-module(test_app).

%%% INCLUDES
-include_lib("kernel/include/logger.hrl").

%%% EXTERNAL EXPORTS
-export([init/1, terminate/1]).

%%% INTROSPECTION EXPORTS
-export([introspection_points/0]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
init(CounterRef) ->
    nintrospection:do(test_app, init, [CounterRef]),
    ok.

terminate(CounterRef) ->
    nintrospection:do(test_app, terminate, [CounterRef]),
    ok.

introspection_points() ->
    [
        init,
        terminate
    ].

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
