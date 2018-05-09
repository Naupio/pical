%%%-------------------------------------------------------------------
%% @doc np_tcp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(np_tcp_sup).
-author("Naupio Z.Y. Huang").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
                strategy => rest_for_one,
                intensity => 10,
                period => 1
                    },
    ChildSpec = [#{
                    id => np_tcp_server,
                    start => {np_tcp_server,start_link,[]},
                    restart => permanent,
                    shutdown => 30*1000,
                    type => worker,
                    modules => [np_tcp_server]
                }],
    {ok, { SupFlags, ChildSpec} }.

%%====================================================================
%% Internal functions
%%====================================================================
