%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 22 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(offset_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

offset_limit_test() ->
    %% offset 5
    ?assert(mhub_queue:offset_limit(10, 5) == lists:seq(5,10)),
    %% offset 10 but only 5 messages in queue
    ?assert(mhub_queue:offset_limit(5, 10) == lists:seq(0,5)),
    ?assert(mhub_queue:offset_limit(5, 5) == lists:seq(0,5)),
    ?assert(mhub_queue:offset_limit(5, 0) == []),
    ?assert(mhub_queue:offset_limit(5, -2) == lists:seq(0,1)).


-endif.
