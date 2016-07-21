%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(mhub).

-define(PUB, <<"pub">>).
-define(SUB, <<"sub">>).
-define(MESSAGE, <<"message">>).
-define(OFFSET, <<"offset">>).

%% API
-export([parse/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse(Data, Client) when is_binary(Data) ->
    R = jiffy:decode(Data, [return_maps]),
    Result = api(R, Client),
    jiffy:encode(Result);

parse(Data, _Client) ->
    {error, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

api(#{?PUB := Queue, ?MESSAGE := Message}, _Client) ->
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:pub(Pid, Message);
api(#{?SUB := Queue, ?OFFSET := Offset}, Client) ->    
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:sub(Pid, Client, Offset);
api(#{?SUB := Queue}, Client) ->    
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:sub(Pid, Client);
api(Err, _Client) ->
    error_logger:error_msg("API Error: unsupported message ~p~n", [Err]),
    <<"Unupported operation">>.

