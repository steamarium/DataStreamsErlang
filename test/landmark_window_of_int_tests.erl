-module(landmark_window_of_int_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/data.hrl").
-include("../include/query.hrl").
-record(state, { next = 1 :: integer(),
	         elements = dict:new() :: dict:dict() %% a dictionary of {Int, #data{}}
	       }).

server_test() ->
    {ok, Pid} = landmark_window_of_int:start_link(),
    SyncCall = gen_server:call(Pid, any),
    ?assertEqual({drop, any}, SyncCall),
    Terminated = gen_server:stop(Pid),
    ?assertEqual(ok, Terminated),
    ok.

expect_count(N, Pid) when is_integer(N) andalso is_pid(Pid) ->
    Count = gen_server:call(Pid, #query{ term= count }),
    Query = gen_server:call(Pid, #query{ term= inspect }),
    ?assertEqual(N, Count),
    ?assertEqual(N, Query#state.next - 1),
    ?assertEqual(N, dict:size(Query#state.elements)),
    ok.

windowing_test() ->
    {ok, Pid} = landmark_window_of_int:start_link(),
    gen_server:call(Pid, #data{ type= int, value= 100 }),
    expect_count(1, Pid),
    gen_server:call(Pid, #data{ type= int, value= 99 }),
    expect_count(2, Pid),
    gen_server:call(Pid, #data{ type= int, value= 98 }),
    expect_count(3, Pid),
    ok = gen_server:stop(Pid),
    ok.
     
    
