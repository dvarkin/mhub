-module(mhub_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    {ok, _} = ranch:start_listener(hub_tcp, 10,
				   ranch_tcp, [{port, 5555}], 
				   mhub_tcp, []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    MhubQueueRegister = 
	#{id => mhub_queue_register,
	  start => {mhub_queue_register, start_link, []},
	  restart => permanent,
	  shutdown => 5000, 
	  type => worker,
	  modules => [mhub_queue_register]
	 },

    MhubQueueSup = 
	#{id => mhub_queue_sup,
	  start => {mhub_queue_sup, start_link, []},
	  restart => permanent,
	  shutdown => 5000, 
	  type => supervisor,
	  modules => [mhub_queue_sup]
	 },

    Procs = [MhubQueueRegister, MhubQueueSup],
    {ok, {{one_for_one, 1, 5}, Procs}}.
