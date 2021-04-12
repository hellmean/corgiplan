-module(corgiplan_job_test).

-include_lib("eunit/include/eunit.hrl").

-include_lib("corgiplan_lib/include/corgiplan_job.hrl").

corgiplan_job_init_test() ->
    {ok, StateManager} = dummy_job_state_manager:start_link(),
    {ok, #corgiplan_job{current_attempts_count = CurrentAttemptsCounts, armed_execution_time = ArmedExecutionTime, job_server_name = corgiplan_dummy_job}, TimeUntilExecution} = corgiplan_job:init(corgiplan_dummy_job),
    {_Date, {_Hour, Minute, _Second}} = ArmedExecutionTime,
    ?assert(TimeUntilExecution =< 5*60*1000),
    ?assertEqual(0, Minute rem 5),
    ?assertEqual(3, CurrentAttemptsCounts),
    stopped = dummy_job_state_manager:stop(StateManager).
