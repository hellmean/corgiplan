-module(corgiplan_dummy_job).

-behaviour(corigplan_job).

-export([inception_time/0, crontab_schedule/0, max_retries/0]).

inception_time() -> 
    LocalTime = calendar:local_time(),
    [Result] = calendar:local_time_to_universal_time_dst(LocalTime),
    Result.

crontab_schedule() -> "*/5 * * * *".

max_retries() -> 3.