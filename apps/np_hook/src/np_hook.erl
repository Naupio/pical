-module(np_hook).
-include("np_hook.hrl").

-behaviour(gen_server).

-record(np_hook_state,{}).

-define(DEFAULT_STATE, #np_hook_state{}). 
% -define(DEFAULT_STATE, #{}). 

-define(SERVER, ?MODULE).

% callback function
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-export([start/0, start_link/0]).

% API
-export([
      add/4
    , delete/4
    , foreach_run/2
    , fold_run/3
        ]).

add(Hook,Module,Function,IdentifyID) ->
    gen_server:call(?MODULE, {add, Hook, Module, Function, IdentifyID}).

delete(Hook,Module,Function,IdentifyID) ->
    gen_server:call(?MODULE, {delete, Hook, Module, Function, IdentifyID}).

foreach_run(Hook,Args) ->
    case ets:lookup(?HOOK_SVR, Hook) of
        [{Hook, HookMsgLists}] ->
            lists:foreach(fun(HookMsg) -> 
                    foreach_run_1(Hook,HookMsg,Args)
                end
                ,lists:sort(HookMsgLists));
        [] ->
            ok
    end. 

fold_run(Hook,InitVal,Args) ->
    case ets:lookup(?HOOK_SVR, Hook) of
        [{Hook, HookMsgLists}] ->
            lists:foldl(fun(HookMsg,InPut) ->
                        OutPut = fold_run_1(Hook,HookMsg,InPut,Args),
                        case OutPut of
                            'EXIT' ->
                                InPut;
                            _ -> OutPut
                        end
                    end
                    ,InitVal
                    ,lists:sort(HookMsgLists));
        [] ->
            InitVal
    end.

start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?HOOK_SVR, [named_table,set,protected,{read_concurrency,true}]),
    {ok,?DEFAULT_STATE}.


handle_call({add, Hook, Module, Function, IdentifyID}, _From, _State) ->
    HookMsg = {IdentifyID, Module, Function},
    Reply = case ets:lookup(?HOOK_SVR, Hook) of
        [] ->
            HMs = [HookMsg],
            ets:insert(?HOOK_SVR, {Hook,HMs}),
            ok;
        [{Hook,OldHMs}] ->
            NewHMs = [HookMsg|lists:delete(HookMsg,OldHMs)],
            ets:insert(?HOOK_SVR, {Hook,NewHMs}),
            ok
    end,
    {reply, Reply, _State};


handle_call({delete, Hook, Module, Function, IdentifyID}, _From, _State) ->
    HookMsg = {IdentifyID, Module, Function},
    Reply = case ets:lookup(?HOOK_SVR, Hook) of
        [] ->
            notdoing,
            ok;
        [{Hook,OldHMs}] ->
            NewHMs = lists:delete(HookMsg,OldHMs),
            ets:insert(?HOOK_SVR, {Hook,NewHMs}),
            ok
    end,
    {reply, Reply, _State};

handle_call(_Msg, _From, _State) ->
    error_logger:error_msg("~n~n module *~p* unknow  *CALL* message:  ~p   which *From*:  ~p   with *State* ~p ~n~n", [?MODULE,_Msg, _From, _State]),
    {noreply, _State}.
    
handle_cast(_Msg, _State) ->
    error_logger:error_msg("~n~n module *~p* unknow  *CAST* message:  ~p   with *State* ~p ~n~n", [?MODULE,_Msg, _State]),   
    {noreply, _State}.
    
handle_info(_Msg, _State) ->
    error_logger:error_msg("~n~n module *~p* unknow  *INFO* message:  ~p   with *State* ~p ~n~n", [?MODULE, _Msg, _State]),
    {noreply, _State}.
    
terminate(_Reson, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    ok.
    

foreach_run_1(Hook, HookMsg, Args) ->
    {_Identify,Module,Function} = HookMsg,
    safe_apply(Hook, Module, Function, Args).

fold_run_1(Hook, HookMsg, InPut, Args) ->
    {_Identify,Module,Function} = HookMsg,
    Result = safe_apply(Hook, Module, Function, [InPut|Args]),
    Result.

safe_apply(Hook, Module, Function, Args) ->
    try 
        if is_function(Function) ->
		    apply(Function, Args);
        true ->
		    apply(Module, Function, Args)
	end
    catch E:R when E /= exit; R /= normal ->
	    error_logger:msg("Hook ~p crashed when running ~p:~p/~p:~n"
		       "** Reason = ~p~n"
		       "** Arguments = ~p",
		       [Hook, Module, Function, length(Args),
			{E, R, get_stacktrace()}, Args]),
	    'EXIT'
    end.

get_stacktrace() ->
    [{Mod, Fun, Loc, Args} || {Mod, Fun, Args, Loc} <- erlang:get_stacktrace()].