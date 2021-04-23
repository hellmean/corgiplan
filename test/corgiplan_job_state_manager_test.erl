-module(corgiplan_job_state_manager_test).

-include_lib("eunit/include/eunit.hrl").

-include_lib("corgiplan_lib/include/corgiplan_job.hrl").

register_arm_execute_test_() ->
    {timeout,
     130,
     fun() ->
        ok = mnesia:start(),
        {ok, Sup} = corgiplan_sup:start_link(),
        corgiplan_job:start_link(corgiplan_dummy_job, {global, corgiplan_job_state_manager}),
        F = fun() -> mnesia:read({corgiplan_job, corgiplan_dummy_job}) end,
        {atomic, [Job]} = mnesia:transaction(F),
        ?debugFmt("~p~n", [Job]),
        ?assertEqual(corgiplan_dummy_job, Job#corgiplan_job.job_server_name),
        timer:sleep(1000 * 125),
        ?assertEqual("Hello\nHello\n", ?capturedOutput),
        {ok, ExecutionResults} = corgiplan_job_state_reader:get_execution_results({global, corgiplan_job_state_reader}, corgiplan_dummy_job),
        ?assertEqual(2, length(ExecutionResults)),
        exit(Sup, normal)
     end}.
