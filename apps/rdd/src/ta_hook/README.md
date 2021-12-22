# ta_hook
An simple hook system for Erlang

# LICENSE
- The [MIT License](./LICENSE)  

```
# Run
- go into the project's src dir.  
`cd path/to/project_root/src`

- edit code file, for example
```erlang
-module(simple_hook_test).

-include_lib("ta_hook/include/ta_hook.hrl").

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
```
- go into the project's root dir and use rebar3 compile to run it with repl.  
`rebar3 shell`

- start the ta_hook application and run test case. 
```erlang
1> application:start(ta_hook).
2> simple_hook_test:foreach_main_test().
3> simple_hook_test:fold_main_test().
```
