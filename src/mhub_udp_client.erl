%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 20 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>

-module(mhub_udp_client).

-export([send/1]).

send(N) when is_binary(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, "localhost", 4444, N),
    Value = receive
                {udp, Socket, _, _, Bin} ->
		    Bin
            after 10000 ->
                    0
            end,
    gen_udp:close(Socket),
    Value.


