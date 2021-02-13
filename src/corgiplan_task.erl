-module(corgiplan_task).

-behaviour(gen_server).

-callback init() -> {ok, State :: term()} | {stop, Reason :: term()}.
-callback handle_result(State :: term(), Result :: term()) ->
                           {ok, NewState :: term()} | {fail, Reason :: term()}.

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([report_result/2, stop/1]).

%% API

report_result(From, Result) ->
    gen_server:cast(From, {report_result, Result}).

stop(Name) ->
    gen_server:call(Name, stop).

%% gen_server

init(TaskModuleName) ->
    TaskModuleName:init().

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({report_result, Result}, TaskModuleName) ->
    {ok, _NewState} = TaskModuleName:handle_result([], Result),
    {noreply, TaskModuleName}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
