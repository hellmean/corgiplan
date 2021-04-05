%%%-------------------------------------------------------------------
%% @doc corgiplan public API
%% @end
%%%-------------------------------------------------------------------

-module(corgiplan_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = mnesia:start(),
    corgiplan_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
