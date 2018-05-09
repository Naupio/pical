-module(np_tcp_ws).
-author("Naupio Z.Y. Huang").
-websocket("rfc6455").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, teminate/2, code_change/3]).

-behaviour(np_tcp_protocol).
-export([start_tcp/4]).

-export([start_link/4]).

start_tcp(Ref, Socket, ProModOpt, OtherOpt) ->
    ?MODULE:start_link(Ref, Socket, ProModOpt, OtherOpt).

start_link(Ref, Socket, ProModOpt, OtherOpt) ->
    gen_server:start_link(?MODULE, [Ref, Socket, ProModOpt, OtherOpt], []).

init([Ref, ClientSocket, ProModOpt, OtherOpt]) ->
    self() ! init,
    np_tcp_util:setopts(ClientSocket, [{active, once}]),
    WsMod = lists:keyfind(ws_mod, 1, ProModOpt),
    State = #{client_socket => ClientSocket
            , ref => Ref
            , pro_mod_opt => ProModOpt
            , other_opt => OtherOpt
            , handshake => false
            , ws_mod => WsMod
            },
    {ok, State}.

handle_call(_Msg, _From, _State) ->
    {reply, _Msg, _State}.


handle_cast({send, Data}, #{client_socket := ClientSocket} = State) ->
    np_tcp_util:send(ClientSocket, Data),
    {noreply, State};

handle_cast(_Msg, _State) ->
    {noreply, _State}.
    
handle_info(init, #{ws_mod := WsMod
                    , client_socket := ClientSocket
                    , ref := Ref
                    , pro_mod_opt := ProModOpt
                    , other_opt := OtherOpt} = State) ->
    {ok, WsData} = WsMod:init([Ref, ClientSocket, ProModOpt, OtherOpt]),
    {noreply, State#{ws_data => WsData}};

handle_info({tcp, ClientSocket, Data}, #{client_socket := ClientSocket, handshake := HandShake} = State) ->
    np_tcp_util:setopts(ClientSocket,[{active, once}]),
    NewState = case HandShake of
                false ->
                    NewHandshake = ws_handshake(Data),
                    if 
                        NewHandshake =:= HandShake ->
                            State;
                        true ->
                            State#{handshake := NewHandshake}
                    end;
                true ->
                    WsData = handle_ws_data(Data, State),
                    State#{ws_data := WsData}
            end,
    {noreply, NewState};

handle_info(_Msg, _State) ->
    {noreply, _State}.

teminate(_Reson, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    ok.

ws_handshake(BinData) ->
    try 
        ListData = binary_to_list(BinData),
        SecWsKey = list_to_binary(
                    lists:last(
                    string:tokens(
                    hd(
                    lists:filter(
                                fun(S) ->
                                    lists:prefix("Sec-WebSocket-Key:", S)
                                end,
                                string:tokens(ListData, "\r\n")
                                )
                        ), ": "))),
        
        SecAfterHash = base64:encode(crypto:hash(sha, <<SecWsKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
        HandshakeReturn =
            ["HTTP/1.1 101 Switching Protocols\r\n",
            "connection: Upgrade\r\n",
            "upgrade: websocket\r\n",
            "sec-websocket-accept: ", SecAfterHash, "\r\n",
            "\r\n"],
        tcp_send(self(), HandshakeReturn),
        true
    of
        true -> true
    catch  
        _E:_Err -> false
    end.

handle_ws_data(Data, #{ws_mod := WsMod} = State) ->

    handle_tcp_data(Data).

handle_tcp_data(Data) ->
    error_logger:info_msg("~n receive: ~w ~n",[Data]),
    [].

tcp_send(ClientPID, Data) ->
    gen_server:cast(ClientPID, {send, Data}).