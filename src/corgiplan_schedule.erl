-module(corgiplan_schedule).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-include("corgiplan_job.hrl").

-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Helper functions

% get_next_execution_time(JobServerName) ->
%     MnesiaResult = mnesia:read(corgiplan_job, JobServerName),
%     case MnesiaResult of
%         [] ->
%             JobServerName:inception_time();
%         [PrevExecTime] ->
%             cron_ops:add_interval(PrevExecTime, JobServerName:crontab_schedule())
%     end.

%% API

start(Name) ->
    supervisor:start_child({global, corgiplan_sup}, Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%register_job(State, JobServerName) -> [].
    %%lookup mnesia for latest know execution time
    % NextExecutionTime = get_next_execution_time(JobServerName),
    % NewState = corgiplan_queue:insert(NextExecutionTime, JobServerName, State),
    % NewState.    %%if no record is found access corgiplan_job:init_time, or something along these lines

                 %gen_server:call(?MODULE, {register_job, JobServerName}).

%% gen_server

init(_Args) ->
    {ok, corgiplan_queue:new()}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({resgister_job, JobServerName}, _From, State) ->
    %NewState = register_job(State, JobServerName),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
