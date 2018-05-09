-module(np_tcp).
-author("Naupio Z.Y. Huang").

-export([start_listener/5
        ,start_websocket/5]).

start_listener(Ref, LisOpt, ProMod, ProModOpt, OtherOpt) ->
    ChildSpec = #{id => {np_tcp_listener, Ref}
                   , start => {np_tcp_listener, start_link, [Ref, LisOpt, ProMod, ProModOpt, OtherOpt]}
                   , restart => permanent
                   , shutdown => infinity
                   , type => worker
                   , modules => [np_tcp_listener]
                   },
    supervisor:start_child(np_tcp_sup, ChildSpec).

start_websocket(Ref, LisOpt, WsMod, WsModOpt, OtherOpt) ->
    ChildSpec = #{id => {np_tcp_listener, Ref}
                   , start => {np_tcp_listener, start_link, [Ref, LisOpt, np_tcp_ws, [{ws_mod, WsMod} |WsModOpt], OtherOpt]}
                   , restart => permanent
                   , shutdown => infinity
                   , type => worker
                   , modules => [np_tcp_listener]
                   },
    supervisor:start_child(np_tcp_sup, ChildSpec).