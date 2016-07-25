%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc 
%%% Entry point for MHUB protocol. 
%%% Parse binary JSON data, end return correct map (in case of synchronus operations) or ok in async. 
%%% @end
%%% Created : 21 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(mhub_protocol).

%% Main keywords for JSON protocol

-define(MESSAGE, <<"message">>).
-define(OFFSET, <<"offset">>).
-define(MARKER, <<"marker">>).

%% Actions 

-define(PUB, <<"pub">>).
-define(SUB, <<"sub">>).
-define(GET, <<"get">>).


-export([parse/2]).

%%--------------------------------------------------------------------
%% @doc Parse Bnary JSON data. Invoke internal 
%% response with ok in case of SUB and PUB - async operations
%% resposne with map() in case of sync operations (GET)
%% response map consits of:
%% "queue" => QueueName :: bianry() - Name of the queue
%% "messages" => [Message] | Message - Messages of the queue. 
%% 1 Message will be returned in case of PUB operation. Get always returns list.
%% "marker" => Marker :: pos_number() - Position of the last message in queue. 
%% @spec parse(Data :: binary(), Client :: pid()) -> map() | ok
%% @end
%%--------------------------------------------------------------------

-spec parse(Data :: binary(), Client :: pid()) -> map() | ok.

parse(Data, Client) when is_binary(Data) ->
    R = jiffy:decode(Data, [return_maps]),
    protocol(R, Client);

parse(Data, _Client) ->
    {error, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% PROTOCOL
%%%===================================================================

protocol(#{?PUB := Queue, ?MESSAGE := Message}, _Client) ->
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:pub(Pid, Message);
protocol(#{?GET := Queue, ?OFFSET := Offset}, Client) ->    
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:get_offset(Pid, Client, Offset);
protocol(#{?GET := Queue, ?MARKER := Marker}, Client) ->    
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:get_marker(Pid, Client, Marker);
protocol(#{?SUB := Queue}, Client) ->    
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:sub(Pid, Client);
protocol(Err, _Client) ->
    error_logger:error_msg("PROTOCOL Error: unsupported message ~p~n", [Err]),
     <<"Unupported operation">>.

