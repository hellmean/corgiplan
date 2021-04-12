-module(corgiplan_job_state_manager).

-behaviour(gen_server).

-include_lib("corgiplan_lib/include/corgiplan_job.hrl").
-include("corgiplan_execution_result.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([arm_job_for_execution/1, mark_success/2, start_link/0, stop/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
         terminate/2]).

stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link({global, corgiplan_job_state_manager},
                          corgiplan_job_state_manager,
                          [],
                          []).

arm_job_for_execution(JobServerName) ->
    gen_server:call({global, corgiplan_job_state_manager},
                    {arm_job_for_execution, JobServerName}).

mark_success(JobServerName, ExecutionTimeUTC) ->
    gen_server:call({global, corgiplan_job_state_manager},
                    {mark_success, JobServerName, ExecutionTimeUTC}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    {ok, empty_state}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({arm_job_for_execution, JobServerName}, _From, State) ->
    Transaction =
        fun() ->
           Job = get_or_create_job_state(JobServerName),
           UpdatedJob =
               shift_to_next_exec_point_when_exceed_retry_count(decrement_attempt_count(Job)),
           ok = mnesia:write(UpdatedJob),
           UpdatedJob
        end,
    {atomic, Job} = mnesia:transaction(Transaction),
    {reply, {ok, Job}, State};
handle_call({mark_success, JobServerName, ExecutionTimeUTC}, _From, State) ->
    Transaction =
        fun() ->
           [Job] = mnesia:read({corgiplan_job, JobServerName}),
           SuccessExecutionState =
               get_execution_result(Job#corgiplan_job{armed_execution_time = ExecutionTimeUTC},
                                    success),
           ok = mnesia:write(SuccessExecutionState),
           if ExecutionTimeUTC =:= Job#corgiplan_job.armed_execution_time ->
                  UpdatedJob = update_job_to_next_execution_point(Job),
                  ok = mnesia:write(UpdatedJob),
                  UpdatedJob;
              true -> Job
           end
        end,
    {atomic, Job} = mnesia:transaction(Transaction),
    {reply, {ok, Job}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec decrement_attempt_count(Job :: #corgiplan_job{}) -> #corgiplan_job{}.
decrement_attempt_count(Job = #corgiplan_job{current_attempts_count =
                                                 CurrentAttemptsCount}) ->
    UpdatedAttemptsCount = CurrentAttemptsCount - 1,
    Job#corgiplan_job{current_attempts_count = UpdatedAttemptsCount}.

-spec shift_to_next_exec_point_when_exceed_retry_count(Job :: #corgiplan_job{}) ->
                                                          #corgiplan_job{}.
shift_to_next_exec_point_when_exceed_retry_count(Job =
                                                     #corgiplan_job{current_attempts_count = 0}) ->
    MaxRetryFailedState = get_execution_result(Job, max_retry_failure),
    ok = mnesia:write(MaxRetryFailedState),
    update_job_to_next_execution_point(Job);
shift_to_next_exec_point_when_exceed_retry_count(Job) ->
    Job.

-spec get_or_create_job_state(JobServerName :: term()) -> #corgiplan_job{}.
get_or_create_job_state(JobServerName) ->
    case mnesia:read({corgiplan_job, JobServerName}) of
        [] ->
            corgiplan_job:make_init_job_state(JobServerName);
        [X] ->
            X
    end.

-spec update_job_to_next_execution_point(Job :: #corgiplan_job{}) -> #corgiplan_job{}.
update_job_to_next_execution_point(Job) ->
    Job#corgiplan_job{current_attempts_count = corgiplan_job:max_retries(Job),
                      armed_execution_time = corgiplan_job:get_next_execution_time(Job)}.

-spec get_execution_result(Job :: #corgiplan_job{},
                           Result :: success | marked_as_failure | max_retry_failure) ->
                              #corgiplan_execution_result{}.
get_execution_result(#corgiplan_job{job_server_name = JobServerName,
                                    armed_execution_time = ArmedExecutionTime},
                     Result) ->
    #corgiplan_execution_result{job_server_name = JobServerName,
                                execution_time = ArmedExecutionTime,
                                status = Result}.
