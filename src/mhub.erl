%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(mhub).

%% API
-export([sub/3, pub/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec sub(Client :: port(), Queue :: binary(), Offset ::pos_integer()) -> ok.

sub(Client, Queue, Offset) -> 
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:sub(Pid, Client, Offset).

-spec pub(Queue :: port(), Message :: binary()) -> ok.

pub(Queue, Message) ->
    Pid = mhub_queue_register:get(Queue),
    mhub_queue:pub(Pid, Message).
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
