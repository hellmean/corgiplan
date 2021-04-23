-module(dummy_executor).

-behaviour(corgiplan_executor).

-export([init/0, handle_request/3, start_link/0]).

init() ->
    {ok, dummy_state}.

handle_request(dummy_action, dummy_parameters, dummy_state) ->
    {ok, "hello world", dummy_state}.

start_link() ->
    corgiplan_executor:start_link({local, dummy_executor}, dummy_executor).