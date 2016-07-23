%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2016, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 22 Jul 2016 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(mhub_SUITE).

-compile(export_all).

-define(OK, <<"\"ok\"">>).

-define(PUB1, <<"{\"pub\":\"queue1\",\"message\":\"test message1\"}">>).
-define(PUB2, <<"{\"pub\":\"queue1\",\"message\":\"test message2\"}">>).
-define(PUB3, <<"{\"pub\":\"queue1\",\"message\":\"test message3\"}">>).

-define(SUB, <<"{\"sub\":\"queue1\"}">>).

-define(SUB_OFFSET1, <<"{\"sub\":\"queue1\",\"offset\":1}">>).
-define(SUB_OFFSET2, <<"{\"sub\":\"queue1\",\"offset\":2}">>).
-define(SUB_OFFSET_SIGNED, <<"{\"sub\":\"queue1\",\"offset\":-2}">>).

-define(RESP1, <<"{\"queue\":\"queue1\",\"messages\":\"test message1\"}">>).
-define(RESP2, <<"{\"queue\":\"queue1\",\"messages\":\"test message2\"}">>).
-define(RESP3, <<"{\"queue\":\"queue1\",\"messages\":\"test message3\"}">>).

-define(RESP_OFFSET, <<"{\"queue\":\"queue1\",\"messages\":[\"test message1\",\"test message2\"]}">>). 
-define(RESP_OFFSET3, <<"{\"queue\":\"queue1\",\"messages\":[\"test message3\"]}">>). 


-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:start(jiffy),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(mhub),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [tcp_pub_sub_test1,
     tcp_pub_sub_test2,
     tcp_pub_sub_offset_test,
     udp_protocol_test_case,
     udp_protocol_market_test_case
    ].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------

tcp_pub_sub_test1(_Config) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),
    {ok, Sock1} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),

    %% Subscribtion is sync operation
    gen_tcp:send(Sock1, ?SUB),
    ?OK = recv(Sock1),

    timer:sleep(200),
    gen_tcp:send(Sock, ?PUB1),
    ?OK = recv(Sock),

    ?RESP1 = recv(Sock1),
    gen_tcp:close(Sock),
    gen_tcp:close(Sock1).

tcp_pub_sub_test2(_Config) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),
    {ok, Sock1} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),

    %% Subscribtion is sync operation

    gen_tcp:send(Sock1, ?SUB),

    timer:sleep(200),
    ?OK = recv(Sock1),
    gen_tcp:send(Sock, ?PUB1),
    ?OK = recv(Sock),

    ?RESP1 = recv(Sock1),


    gen_tcp:send(Sock, ?PUB2),
    ?OK = recv(Sock),

    ?RESP2 = recv(Sock1),

    gen_tcp:send(Sock, ?PUB3),
    ?OK = recv(Sock),

    ?RESP3 = recv(Sock1),

    gen_tcp:close(Sock),
    gen_tcp:close(Sock1).


tcp_pub_sub_offset_test(_Config) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),
    {ok, Sock1} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}, {active, false}]),

    gen_tcp:send(Sock, ?PUB1),
    ?OK = recv(Sock),

    gen_tcp:send(Sock, ?PUB2),
    ?OK = recv(Sock),

    gen_tcp:send(Sock1, ?SUB_OFFSET2),
    ?RESP_OFFSET = recv(Sock1),


    gen_tcp:send(Sock, ?PUB3),
    ?OK = recv(Sock),

    timer:sleep(200),
    ?RESP3 = recv(Sock1),
    gen_tcp:close(Sock).

udp_protocol_test_case(_Config) ->
    PUB1 = <<"{\"pub\":\"queue10\",\"message\":\"test message1\"}">>,
    PUB2 = <<"{\"pub\":\"queue10\",\"message\":\"test message2\"}">>,
    PUB3 = <<"{\"pub\":\"queue10\",\"message\":\"test message3\"}">>,

    SUB_OFFSET1 = <<"{\"sub\":\"queue10\",\"offset\":1}">>,
    SUB_OFFSET2 = <<"{\"sub\":\"queue10\",\"offset\":2}">>,
    SUB_OFFSET_SIGNED = <<"{\"sub\":\"queue10\",\"offset\":-2}">>,
    
    RESP_OFFSET = <<"{\"queue\":\"queue10\",\"messages\":[\"test message1\",\"test message2\"]}">>,
    RESP_OFFSET3 = <<"{\"queue\":\"queue10\",\"messages\":[\"test message3\"]}">>,

    ?OK = mhub_udp_client:send(PUB1),
    ?OK = mhub_udp_client:send(PUB2),
    RESP_OFFSET = mhub_udp_client:send(SUB_OFFSET2),
    ?OK = mhub_udp_client:send(PUB3),
    RESP_OFFSET3 = mhub_udp_client:send(SUB_OFFSET1),
    RESP_OFFSET = mhub_udp_client:send(SUB_OFFSET_SIGNED),
    ok.

udp_protocol_market_test_case(_Config) ->
    PUB1 = <<"{\"pub\":\"queue11\",\"message\":\"test message1\"}">>,
    PUB2 = <<"{\"pub\":\"queue11\",\"message\":\"test message2\"}">>,
    PUB3 = <<"{\"pub\":\"queue11\",\"message\":\"test message3\"}">>,
    PUB4 = <<"{\"pub\":\"queue11\",\"message\":\"test message4\"}">>,
    PUB5 = <<"{\"pub\":\"queue11\",\"message\":\"test message5\"}">>,

    SUB_MARKER0 = <<"{\"sub\":\"queue11\",\"marker\":0}">>,
    SUB_MARKER2 = <<"{\"sub\":\"queue11\",\"marker\":2}">>,
    
    RESP_OFFSET = <<"{\"queue\":\"queue11\",\"messages\":[\"test message1\",\"test message2\"]}">>,
    RESP_OFFSET3 = <<"{\"queue\":\"queue11\",\"messages\":[\"test message3\"]}">>,

    ?OK = mhub_udp_client:send(PUB1),
    ?OK = mhub_udp_client:send(PUB2),
    R  = mhub_udp_client:send(SUB_MARKER0),
    #{<<"marker">> := 2} = jiffy:decode(R, [return_maps]),
    ?OK = mhub_udp_client:send(PUB3),
    ?OK = mhub_udp_client:send(PUB4),
    ?OK = mhub_udp_client:send(PUB5),
    R1  = mhub_udp_client:send(SUB_MARKER2),
    #{<<"marker">> := 5} = jiffy:decode(R1, [return_maps]),
    ok.


recv(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            Data;
        {error, closed} -> 
            {error, closed}
    end.
