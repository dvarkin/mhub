%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(mhub_queue).

-behaviour(gen_server).

%% API
-export([start_link/1, pub/2, sub/2, sub/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name :: binary(), 
		timeout :: pos_integer(),
		queue :: map(),
		clients :: map(),
		message_counter :: pos_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec pub(Queue :: pid(), Message :: binary()) -> ok.

pub(Queue, Message) ->
    gen_server:cast(Queue, {pub, Message}).

-spec sub(Queue :: pid(), Client :: port()) -> ok.

sub(Queue, Client) ->
    sub(Queue, Client, 0).

-spec sub(Queue :: pid(), Client :: port(), Offset :: pos_integer()) -> ok.

sub(Queue, Client, Offset) ->
    gen_server:cast(Queue, {sub, Client, Offset}).

start_link(#{qname := Name, timeout := Timeout}) when Timeout > 0 ->
    gen_server:start_link(?MODULE, [Name, Timeout], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Timeout]) ->
    {ok, #state{name = Name, timeout = Timeout, queue = #{}, clients = #{}, message_counter = 0}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({pub, Message}, #state{name = Name, queue = Q, clients = Clients, message_counter = Counter} = State) ->
    error_logger:info_msg("~p  ~p ~n", [Clients, Q]),
    NewQ = maps:put(Counter, Message, Q),
    maps:map(fun(Pid,_V) -> send(Pid, Name, Message) end, Clients),
    %% send to subscribers
    {noreply, State#state{queue = NewQ, message_counter = Counter + 1}};

handle_cast({sub, Client, 0}, #state{clients = Clients} = State) ->
    {noreply, State#state{clients = Clients#{Client => 0}}};

handle_cast({sub, Client, Offset}, #state{name = Name, clients = Clients, queue = Q, message_counter = Counter} = State) ->
    error_logger:info_msg("~p  ~p ~n", [Client, Offset]),
    %% should implement better limits
    Keys = offset_limit(Counter, Offset),
    Messages = maps:with(Keys, Q),
    %error_logger:info_msg("messages by offset ~p~n", [Messages]),
    send(Client, Name, Messages),
    {noreply, State#state{clients = Clients#{Client => 0}}};


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

offset_limit(Counter, Offset) when Counter > Offset ->
    lists:seq(Counter - Offset, Counter);
offset_limit(Counter, Offset) when Counter =< Offset ->
    lists:seq(0, Counter).
    
send(Client, QueueName, Messages) ->
    M = #{<<"queue">> => QueueName, <<"messages">> => Messages},
    Client ! M.


