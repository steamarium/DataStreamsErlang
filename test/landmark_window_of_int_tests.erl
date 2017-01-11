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

windowing_test() ->
    {ok, Pid} = landmark_window_of_int:start_link(),
    gen_server:call(Pid, #data{ type= int, value= 100 }),
    gen_server:cast(Pid, #data{ type= int, value= 99 }),
    Query = gen_server:call(Pid, #query{ term= inspect }),
    ?assertEqual(Query#state.next - 1, dict:size(Query#state.elements)),
    ok = gen_server:stop(Pid),
    ok.
     
    
