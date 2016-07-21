%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(mhub_queue_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_queue/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_queue(Qname :: binary()) -> {ok, pid()}.

start_queue(Qname) ->
    QueueData = #{qname => Qname, timeout => 60000},
    supervisor:start_child(mhub_queue_sup, [QueueData]).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    
    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 1,
		 period => 5},
    
    MhubQueue = #{id => mhub_queue,
	       start => {mhub_queue, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [mhub_queue]},
    
    {ok, {SupFlags, [MhubQueue]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
