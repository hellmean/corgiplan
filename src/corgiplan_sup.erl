%%%-------------------------------------------------------------------
%% @doc corgiplan top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(corgiplan_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags =
        #{strategy => one_for_all,
          intensity => 1,
          period => 5},
    ChildSpecs =
        [#{id => corgiplan_job_state_manager,
           start => {corgiplan_job_state_manager, start_link, []}},
         #{id => corgiplan_job_state_reader,
           start => {corgiplan_job_state_reader, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
