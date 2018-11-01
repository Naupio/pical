%%%-------------------------------------------------------------------
%% @doc ta_hook top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ta_hook_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(HOOK_MOD, ta_hook).
-define(HOOK_STOP_TIME, 30000).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 1},
    ChildSpecs = [
        #{id => ?HOOK_MOD,
          start => {?HOOK_MOD, start_link, []},
          restart => permanent,
          shutdown => ?HOOK_STOP_TIME,
          type => worker,
          modules => [?HOOK_MOD] }
    ],
    {ok, { SupFlags, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
