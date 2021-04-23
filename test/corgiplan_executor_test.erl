-module(corgiplan_executor_test).
-include_lib("eunit/include/eunit.hrl").

dummy_executor_start_link_test() ->
    {ok, Executor} = dummy_executor:start_link(),
    stopped = corgiplan_executor:stop(Executor).

dummy_executor_make_request_test() ->
    {ok, Executor} = dummy_executor:start_link(),
    "hello world" = corgiplan_executor:make_request(Executor, dummy_token, dummy_action, dummy_parameters, 1000),
    stopped = corgiplan_executor:stop(Executor).

dummy_executor_make_multiple_requests_test() ->
    {ok, Executor} = dummy_executor:start_link(),
    "hello world" = corgiplan_executor:make_request(Executor, dummy_token, dummy_action, dummy_parameters, 1000),
    "hello world" = corgiplan_executor:make_request(Executor, dummy_token, dummy_action, dummy_parameters, 1000),
    "hello world" = corgiplan_executor:make_request(Executor, dummy_token, dummy_action, dummy_parameters, 1000),
    stopped = corgiplan_executor:stop(Executor).