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
-export([start_link/1, pub/2, sub/2, get_offset/3, get_marker/3]).

-export([offset_limit/2]).

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
    gen_server:cast(Queue, {sub, Client, 0}).

-spec get_offset(Queue :: pid(), Client :: port(), Offset :: pos_integer()) -> map().

get_offset(Queue, Client, Offset) when is_integer(Offset) ->
    gen_server:call(Queue, {get_offset, Client, Offset});
get_offset(_Queue, _Client, _Offset) ->
    #{<<"error">> => <<"offset should be a number!">>}.

-spec get_marker(Queue :: pid(), Client :: port(), Marker :: pos_integer()) -> map().

get_marker(Queue, Client, Marker) when Marker >= 0 ->
    gen_server:call(Queue, {get_marker, Client, Marker});
get_marker(_Queue, _Client, _Marker) ->
    #{<<"error">> => <<"marker should be a positive number!">>}.

start_link(#{qname := Name, timeout := Timeout}) when Timeout > 0 ->
    gen_server:start_link(?MODULE, [Name, Timeout], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Timeout]) ->
    {ok, #state{name = Name, timeout = Timeout, queue = #{}, clients = #{}, message_counter = 0}}.

handle_call({get_offset, Pid, Offset}, _From, #state{name = Name, clients = Clients, queue = Q, message_counter = Counter} = State) ->
    is_monitor(Pid, maps:is_key(Pid, Clients)),
    Keys = offset_limit(Counter, Offset),
    Messages = [M || {_, M} <- maps:to_list(maps:with(Keys, Q))],
    Reply = make_message(Name, Messages, Counter),
    {reply, Reply, State#state{clients = Clients#{Pid => 0}}};

handle_call({get_marker, Pid, Marker}, 
	    _From, 
	    #state{name = Name, 
		   clients = Clients, 
		   queue = Q, 
		   message_counter = Counter} = State) when Marker < Counter ->
    is_monitor(Pid, maps:is_key(Pid, Clients)),
    Keys = lists:seq(Marker, Counter),
    Messages = [M || {_, M} <- maps:to_list(maps:with(Keys, Q))],
    Reply = make_message(Name, Messages, Counter),
    {reply, Reply, State#state{clients = Clients#{Pid => 0}}};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({pub, Message}, #state{name = Name, queue = Q, clients = Clients, message_counter = Counter} = State) ->
    NewQ = maps:put(Counter, Message, Q),
    maps:map(fun(Pid,_V) -> send(Pid, Name, Message, Counter) end, Clients),
    {noreply, State#state{queue = NewQ, message_counter = Counter + 1}};

handle_cast({sub, Pid, 0}, #state{clients = Clients} = State) ->
    is_monitor(Pid, maps:is_key(Pid, Clients)),
    {noreply, State#state{clients = Clients#{Pid => 0}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% remove client's PID from subscribers
handle_info({'DOWN', _MonitorRef, _Type, ClientPid, _Info}, #state{clients = Q} = State) ->
    NewQ = maps:remove(ClientPid, Q),
    {noreply, State#state{clients = NewQ}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec offset_limit(Counter :: pos_integer(), Counter :: integer()) -> [pos_integer()].

offset_limit(Counter, Offset) when Counter == 0 orelse Offset == 0 ->
    [];
offset_limit(Counter, Offset) when Offset < 0 ->
    UnsignedOffset = abs(Offset),
    case Counter > UnsignedOffset of
	true ->
	    lists:seq(0, abs(Offset) - 1);
	false ->
	    lists:seq(0, Counter)
    end;
offset_limit(Counter, Offset) when Counter > Offset ->
    lists:seq(Counter - Offset, Counter);
offset_limit(Counter, Offset) when Counter =< Offset ->
    lists:seq(0, Counter).

make_message(QueueName, Messages, Marker) ->
    #{<<"queue">> => QueueName, <<"messages">> => Messages, <<"marker">> => Marker}.

send(Client, QueueName, Message, Marker) ->
    %% error_logger:info_msg("~p Messages ~p~n", [M, Client]),
    M = make_message(QueueName, Message, Marker),
    Client ! {msg, M}.

is_monitor(_Pid, true) ->
    ok;
is_monitor(Pid, false) ->
    erlang:monitor(process, Pid).
    

