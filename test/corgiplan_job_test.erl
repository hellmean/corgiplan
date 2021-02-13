-module(corgiplan_job_test).

-include_lib("eunit/include/eunit.hrl").

-record(state,
        {callback_module :: module(),
         fail_count = 0 :: non_neg_integer(),
         next_execution_time :: calendar:rfc3339_string()}).

corgiplan_job_init_test() ->
    {ok, StateManager} = dummy_job_state_manager:start_link(),
    {ok, #state{fail_count = FailCount, next_execution_time = NextExecutionTimeUTC}, TimeUntilExecution} = corgiplan_job:init(corgiplan_dummy_job),
    {_Date, {_Hour, Minute, _Second}} = NextExecutionTimeUTC,
    ?assert(TimeUntilExecution =< 5*60*1000),
    ?assertEqual(0, Minute rem 5),
    ?assertEqual(0, FailCount),
    stopped = dummy_job_state_manager:stop(StateManager).
