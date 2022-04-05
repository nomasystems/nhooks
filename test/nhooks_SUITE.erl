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
%% limitations under the License.
-module(nhooks_SUITE).

%%% INCLUDE FILES

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).
-define(APP_NAME, test_app).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        register,
        register_task_in_a_non_existent_hook,
        deregister_single_task,
        deregister_all_app_tasks,
        deregister_task_in_non_existent_hook,
        execution_of_one_fun_per_hook,
        execution_of_one_mod_fun_per_hook,
        execution_of_several_tasks_per_hook,
        execution_of_several_tasks_per_hook_with_exceptions
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    nhooks:deregister_app_tasks(?APP_NAME),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
register() ->
    [{userdata, [{doc, "Tests the registration of a hook"}]}].

register(_Conf) ->
    ok = nhooks:register_task(?APP_NAME, init, fun(CounterRef) ->
        counters:add(CounterRef, 1, 1)
    end),
    true = [] /= nhooks:consult_tasks(?APP_NAME, init),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(CounterRef) ->
        counters:sub(CounterRef, 1, 1)
    end),
    true = [] /= nhooks:consult_tasks(?APP_NAME, terminate),
    true = [] /= nhooks:consult_tasks(?APP_NAME, init),
    ok.

register_task_in_a_non_existent_hook() ->
    [
        {userdata, [
            {doc, "Tests the registration of a task in a non existent hook"}
        ]}
    ].

register_task_in_a_non_existent_hook(_Conf) ->
    try
        ok = nhooks:register_task(?APP_NAME, non_existent_hook, fun(_CounterRef) -> ok end),
        ct:fail("Registration of a task in a non existent hook didn't fail")
    catch
        error:'non_existent_hook' ->
            ok
    end.

deregister_single_task() ->
    [{userdata, [{doc, "Tests the deregistration of a single task from a hook"}]}].

deregister_single_task(_Conf) ->
    ok = nhooks:register_task(?APP_NAME, init, fun(CounterRef) ->
        counters:add(CounterRef, 1, 1)
    end),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(CounterRef) ->
        counters:sub(CounterRef, 1, 1)
    end),
    ok = nhooks:deregister_task(?APP_NAME, init),
    [] = nhooks:consult_tasks(?APP_NAME, init),
    true = [] /= nhooks:consult_tasks(?APP_NAME, terminate),
    ok = nhooks:deregister_task(?APP_NAME, terminate),
    [] = nhooks:consult_tasks(?APP_NAME, terminate),
    ok.

deregister_all_app_tasks() ->
    [{userdata, [{doc, "Tests the deregistration of all app tasks"}]}].

deregister_all_app_tasks(_Conf) ->
    ok = nhooks:register_task(?APP_NAME, init, fun(CounterRef) ->
        counters:add(CounterRef, 1, 1)
    end),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(CounterRef) ->
        counters:sub(CounterRef, 1, 1)
    end),
    ok = nhooks:deregister_app_tasks(?APP_NAME),
    #{init := [], terminate := []} = nhooks:consult_app_tasks(?APP_NAME),
    ok.

deregister_task_in_non_existent_hook() ->
    [{userdata, [{doc, "Tests the deregistration of a task from a non existent hook"}]}].

deregister_task_in_non_existent_hook(_Conf) ->
    ok = nhooks:deregister_task(?APP_NAME, non_existent_hook).

execution_of_one_fun_per_hook() ->
    [
        {userdata, [
            {doc, "Tests the registration and execution of one fun per hook"}
        ]}
    ].

execution_of_one_fun_per_hook(_Conf) ->
    Counter = counters:new(1, []),
    ok = nhooks:register_task(?APP_NAME, init, fun(CounterRef) ->
        counters:add(CounterRef, 1, 1)
    end),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(CounterRef) ->
        counters:sub(CounterRef, 1, 1)
    end),
    ?APP_NAME:init(Counter),
    1 = counters:get(Counter, 1),
    ?APP_NAME:terminate(Counter),
    0 = counters:get(Counter, 1),
    ok.

execution_of_one_mod_fun_per_hook() ->
    [
        {userdata, [
            {doc, "Tests the registration and execution of one {mod, fun} per hook"}
        ]}
    ].

execution_of_one_mod_fun_per_hook(_Conf) ->
    Counter = counters:new(1, []),
    ok = nhooks:register_task(?APP_NAME, init, {?MODULE, add_one_to_counter}),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(CounterRef) ->
        counters:sub(CounterRef, 1, 1)
    end),
    ?APP_NAME:init(Counter),
    1 = counters:get(Counter, 1),
    ?APP_NAME:terminate(Counter),
    0 = counters:get(Counter, 1),
    ok.

execution_of_several_tasks_per_hook() ->
    [
        {userdata, [
            {doc, "Tests the registration and execution of several tasks per hook"}
        ]}
    ].

execution_of_several_tasks_per_hook(_Conf) ->
    Counter = counters:new(1, []),
    ok = nhooks:register_task(?APP_NAME, init, fun(CounterRef) ->
        counters:add(CounterRef, 1, 1)
    end),
    ok = nhooks:register_task(?APP_NAME, init, fun(CounterRef) ->
        counters:add(CounterRef, 1, 2)
    end),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(CounterRef) ->
        counters:sub(CounterRef, 1, 1)
    end),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(CounterRef) ->
        counters:sub(CounterRef, 1, 2)
    end),
    ?APP_NAME:init(Counter),
    3 = counters:get(Counter, 1),
    ?APP_NAME:terminate(Counter),
    0 = counters:get(Counter, 1),
    ok.

execution_of_several_tasks_per_hook_with_exceptions() ->
    [
        {userdata, [
            {doc, "Tests the registration and execution of several tasks per hook"}
        ]}
    ].

execution_of_several_tasks_per_hook_with_exceptions(_Conf) ->
    Counter = counters:new(1, []),
    ok = nhooks:register_task(?APP_NAME, init, fun(_CounterRef) ->
        erlang:throw("some error")
    end),
    ok = nhooks:register_task(?APP_NAME, init, fun(CounterRef) ->
        counters:add(CounterRef, 1, 2)
    end),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(CounterRef) ->
        counters:sub(CounterRef, 1, 1)
    end),
    ok = nhooks:register_task(?APP_NAME, terminate, fun(_CounterRef) ->
        erlang:error("some error")
    end),
    ?APP_NAME:init(Counter),
    2 = counters:get(Counter, 1),
    ?APP_NAME:terminate(Counter),
    1 = counters:get(Counter, 1),
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
add_one_to_counter(CounterRef) ->
    counters:add(CounterRef, 1, 1).
