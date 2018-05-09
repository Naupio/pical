-module(simple_tcp_example_protocol).

-behaviour(gen_server).
-behaviour(np_tcp_protocol).

%% gen_server callback function
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, teminate/2, code_change/3]).

%% np_tcp_protocol callback function
-export([start_tcp/4]).

%% api
-export([tcp_send/2]).

-define(SERVER, ?MODULE).

start_tcp(Ref, ClientSocket, ProModOpt, OtherOpt) ->
    start_link(Ref, ClientSocket, ProModOpt, OtherOpt).

start_link(Ref, ClientSocket, ProModOpt, OtherOpt) ->
    gen_server:start_link(?MODULE, [Ref, ClientSocket, ProModOpt, OtherOpt], []).

init([Ref, ClientSocket, ProModOpt, OtherOpt]) ->
    self() ! init,
    np_tcp_util:setopts(ClientSocket,[{active, once}]),
    State = #{client_socket => ClientSocket
            , ref => Ref
            , pro_mod_opt => ProModOpt
            , other_opt => OtherOpt
            },
    {ok, State}.

handle_call(get_client_socket, _From, #{client_socket := ClientSocket}=State) ->
    {reply,ClientSocket,State};

handle_call(_Msg, _From, _State) ->
    {reply, _Msg, _State}.


handle_cast({send,Data}, #{client_socket := ClientSocket}=State) ->
    np_tcp_util:send(ClientSocket,Data),
    {noreply, State};

handle_cast(_Msg, _State) ->
    {noreply, _State}.
    
handle_info(init, _State) ->
    {noreply, _State};

handle_info({tcp,ClientSocket,Data}, #{client_socket := ClientSocket}=State) ->
    np_tcp_util:setopts(ClientSocket,[{active, once}]),
    ReplyData = handle_tcp_data(Data),
    tcp_send(self(),ReplyData),
    {noreply, State};

handle_info(_Msg, _State) ->
    {noreply, _State}.

teminate(_Reson, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    ok.

handle_tcp_data(Data) ->
    error_logger:info_msg("~n receive: ~w ~n",[Data]),
    Data.

tcp_send(ClientPID,Data) ->
    gen_server:cast(ClientPID,{send,Data}).