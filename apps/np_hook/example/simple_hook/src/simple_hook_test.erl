-module(simple_hook_test).

-include_lib("np_hook/include/np_hook.hrl").

-export([foreach_main_test/0,fold_main_test/0]).

foreach_main_test() ->
    io:format("~n~n  foreach_main_test start  ~n~n"),
    FunNum = 10,
    Fun = fun(X) ->
                fun(_) -> io:format("~n~n  foreach_test_fun_~w  ~n~n",[X]) end
            end,

    Hook = foreach_main_test,
    Module = global,

    % add 10 function to hook
    lists:foreach(
            fun(IdentifyID) ->
                ?HOOK_SVR:add(Hook,Module,Fun(IdentifyID),IdentifyID)
            end,
            lists:seq(1,FunNum)),

    Args = [nothing],
    ?HOOK_SVR:foreach_run(Hook,Args),

    % delete 5 function by hook
    lists:foreach(
        fun(IdentifyID) ->
            ?HOOK_SVR:delete(Hook,Module,Fun(IdentifyID),IdentifyID)
        end,
        lists:seq(1,FunNum div 2)),

    ?HOOK_SVR:foreach_run(Hook,Args),
    io:format("~n~n  foreach_main_test end  ~n~n"),
    ok.

fold_main_test() ->
    io:format("~n~n  fold_main_test start  ~n~n"),
    FunNum = 10,
    Fun = fun(X) ->
                fun(InPut) -> X+InPut end
            end,

    Hook = fold_main_test,
    Module = global,

    % add 10 function to hook
    lists:foreach(
            fun(IdentifyID) ->
                ?HOOK_SVR:add(Hook,Module,Fun(IdentifyID),IdentifyID)
            end,
            lists:seq(1,FunNum)),

    InitVal = 0,
    Args = [],
    FoldRes1 = ?HOOK_SVR:fold_run(Hook,InitVal,Args),
    io:format("~n~n  FoldRes1:~w  ~n~n",[FoldRes1]),

    % delete 5 function by hook
    lists:foreach(
        fun(IdentifyID) ->
            ?HOOK_SVR:delete(Hook,Module,Fun(IdentifyID),IdentifyID)
        end,
        lists:seq(1,FunNum div 2)),

    FoldRes2 = ?HOOK_SVR:fold_run(Hook,InitVal,Args),
    io:format("~n~n  FoldRes2:~w  ~n~n",[FoldRes2]),
    io:format("~n~n  fold_main_test end  ~n~n"),
    ok.



