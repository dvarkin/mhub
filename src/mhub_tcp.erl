-module(mhub_tcp).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, 120000). %2m

-record(state, {socket, transport}).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
			  #state{socket=Socket, transport=Transport},
			  ?TIMEOUT).

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    R = try mhub:parse(Data, self()) of
	    Result -> Result
	catch 
	    _ -> jiffy:encode(#{error => <<"Invalid JSON">>, received => Data})
	end,
    self() ! {msg, R},
    {noreply, State, ?TIMEOUT};
handle_info({msg, Message}, #state{socket = Socket, transport = Transport} = State ) ->
%%    error_logger:info_msg("Send from socket ~p~n", [Message]),
    Transport:send(Socket, Message),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal.

%% reverse_binary(B) when is_binary(B) ->
%% 	[list_to_binary(lists:reverse(binary_to_list(
%% 		binary:part(B, {0, byte_size(B)-2})
%% 	))), "\r\n"].
