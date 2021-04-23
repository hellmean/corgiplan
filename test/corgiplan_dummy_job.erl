-module(corgiplan_dummy_job).

-behaviour(corgiplan_job).

-export([inception_time/0, crontab_schedule/0, max_retries/0, cooldown_millis/0, execution_plan/1]).

cooldown_millis() -> 10000.

inception_time() -> 
    LocalTime = calendar:local_time(),
    [Result] = calendar:local_time_to_universal_time_dst(LocalTime),
    Result.

crontab_schedule() -> "*/1 * * * *".

max_retries() -> 3.

execution_plan(_ExecutionTime) ->
    io:format("Hello~n"),
    ok.