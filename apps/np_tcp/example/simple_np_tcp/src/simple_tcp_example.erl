-module(simple_tcp_example).

-export([run_listener/0,spawn_conn/1]).

run_listener() ->
    Ref = simple_tcp_example,
    LisOpt = [binary,{port,18080},{packet,0},{active,false},{ip,{127,0,0,1}}],
    ProMod = simple_tcp_example_protocol,
    ProModOpt = [],
    OtherOpt = [],
    np_tcp:start_listener(Ref, LisOpt, ProMod, ProModOpt, OtherOpt).

spawn_conn(TestNum) when is_number(TestNum) andalso (TestNum > 0) ->
    lists:foreach( fun(MsgNum) ->
        spawn(fun() ->
            {ok,Socket} = np_tcp_util:connect({127,0,0,1},18080,[{active,false}]),
            np_tcp_util:send(Socket,<<MsgNum>>),
            error_logger:info_msg("~n send: ~w ~n",[MsgNum])
        end)
    end,
    lists:seq(1,TestNum))
    .