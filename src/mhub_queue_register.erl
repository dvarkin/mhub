%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%% Registrate queues. 
%%% Contain table with BinaryQueue name -> Process pid() identification. 
%%% @end
%%% Created : 21 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(mhub_queue_register).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {queues :: map()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% Start new process or return pid() of existing process by Queue Name. 
%% @spec get(Queue :: binary()) -> pid()
%% @end
%%--------------------------------------------------------------------

-spec get(Queue :: binary()) -> pid().

get(Queue) ->
    gen_server:call(?SERVER, {get, Queue}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{queues = #{}}}.

handle_call({get, Queue}, _From, #state{queues = Q} = State) ->
    
    Pid = case maps:get(Queue, Q, undefined) of
	      undefined ->
		  {ok, QPid} = mhub_queue_sup:start_queue(Queue),
		  erlang:monitor(process, QPid),
		  QPid;
	      QPid ->
		  QPid
	  end,
    {reply, Pid, State#state{queues = Q#{Queue => Pid}}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, QueuePID, _Info}, #state{queues = Q} = State) ->
    NewQ = maps:remove(QueuePID, Q),
    {noreply, State#state{queues = NewQ}};
handle_info(_Info, State) ->
    error_logger:info_msg("Unexpeted message in register ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
