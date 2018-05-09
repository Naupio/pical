%%%-------------------------------------------------------------------
%% @doc npdata public API
%% @end
%%%-------------------------------------------------------------------

-module(npdata_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    npdata_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================