-module(dummy_job_state_manager).

-behaviour(gen_server).

-include("../src/corgiplan_job.hrl").

%% API
-export([stop/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).
stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link({global, corgiplan_job_state_manager}, dummy_job_state_manager, [], []).

init(_Args) ->
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({arm_job_for_execution, corgiplan_dummy_job}, _From, State) ->
    Job = #corgiplan_job{job_server_name=corgiplan_dummy_job, last_correct_execution_utc = corgiplan_dummy_job:inception_time(), current_attempts_count=1},
    {reply, {ok, Job}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
