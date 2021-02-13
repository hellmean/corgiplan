-module(corgiplan_job_state_manager).

-behaviour(gen_server).

-include("corgiplan_job.hrl").
-include("corgiplan_execution_result.hrl").

%% API
-export([stop/1, start_link/0, arm_job_for_execution/1, mark_success/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link({global, corgiplan_job_state_manager},
                          corgiplan_job_state_manager,
                          [],
                          []).

arm_job_for_execution(JobServerName) ->
    gen_server:call({global, corgiplan_job_state_manager}, {arm_job_for_execution, JobServerName}).

mark_success(JobServerName, ExecutionTimeUTC) ->
    gen_server:call({global, corgiplan_job_state_manager}, {mark_success, JobServerName, ExecutionTimeUTC}).

%% gen_server callbacks

init(_Args) ->
    {ok, empty_state}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({arm_job_for_execution, JobServerName}, _From, State) ->
    Transaction =
        fun() ->
           Job = case mnesia:read({corgiplan_job, JobServerName}) of
                     [] -> generate_init_job_state(JobServerName);
                     [X] -> X
                 end,
           UpdatedAttemptsCount = Job#corgiplan_job.current_attempts_count + 1,
           UpdatedJob = if
               UpdatedAttemptsCount > JobServerName:max_retries() ->
                    ExecutionTimeUTC = Job#corgiplan_job.armed_execution_time
                    ExecutionResult = get_execution_result(JobServerName, ExecutionTimeUTC, max_retry_failure),
                    ok = mnesia:write(ExecutionResult);
                    update_job_to_next_execution_point(Job),
                true ->
                    Job#corgiplan_job{current_attempts_count = UpdatedAttemptsCount},
            end
            ok = mnesia:write(UpdatedJob),
            UpdatedJob
        end,
    Job = mnesia:transaction(Transaction),
    {reply, {ok, Job}, State};
handle_call({mark_success, JobServerName, ExecutionTimeUTC}, _From, State) ->
    Transaction =
        fun() ->
           [Job] = mnesia:read({corgiplan_job, JobServerName}),
           ExecutionResult = get_execution_result(JobServerName, ExecutionTimeUTC, success),
           ok = mnesia:write(ExecutionResult),
           if ExecutionTimeUTC =:= Job#corgiplan_job.armed_execution_time ->
                  UpdatedJob = update_job_to_next_execution_point(Job),
                  ok = mnesia:write(UpdatedJob),
                UpdatedJob;
              true -> Job
           end
        end,
    Job = mnesia:transaction(Transaction),
    {reply, {ok, Job}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% helper functions

-spec update_job_to_next_execution_point(Job :: #corigplan_job{}) -> #corgiplan_job{}.
update_job_to_next_execution_point(Job) ->
    Job#corgiplan_job{current_attempts_count = 1,
                    armed_execution_time = get_next_execution_time(Job)},

-spec generate_init_job_state(JobServerName :: term()) -> #corgiplan_job{}.
generate_init_job_state(JobServerName) ->
    Job = #corgiplan_job{job_server_name = JobServerName,
                         armed_execution_time = JobServerName:inception_time(),
                         current_attempts_count = 0},
    Job.

-spec get_next_execution_time(Job :: #corgiplan_job{}) -> calendar:datetime1970().
get_next_execution_time(#corgiplan_job{job_server_name = JobServerName,
                                       armed_execution_time = CurrentExecutionTimeUTC}) ->
    ParsedCrontabSpec =
        cron_ops:time_specs(
            JobServerName:crontab_schedule()),
    cron_ops:next(CurrentExecutionTimeUTC, ParsedCrontabSpec).

-spec get_execution_result(JobServerName :: term(), ExecutionTimeUTC :: calendar:datetime1970, Result :: success | marked_as_failure | max_retry_failure) -> #corgiplan_execution_result{}.
get_execution_result(JobServerName, ExecutionTimeUTC, Result) ->
    corgiplan_execution_result{job_server_name = JobServerName,
                                                execution_time = ExecutionTimeUTC,
                                                status = Result}.