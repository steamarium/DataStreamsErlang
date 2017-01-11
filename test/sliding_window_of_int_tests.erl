-module(sliding_window_of_int_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/data.hrl").
-include("../include/query.hrl").
-record(state, { next = 1 :: integer(),
		 size :: integer(),
	         elements = dict:new() :: dict:dict() %% a dictionary of {Int, #data{}}
	       }).
-define(module, sliding_window_of_int).

searver_test() ->
    {ok, Pid} = ?module:start_link(0),
    SyncCall = gen_server:call(Pid, any),
    ?assertEqual({drop, any}, SyncCall),
    Terminated = gen_server:stop(Pid),
    ?assertEqual(ok, Terminated),
    ok.

count(#state{}= Query) ->
    if Query#state.next - 1 < Query#state.size ->
	    Query#state.next - 1;
       true ->
	    Query#state.size
    end.

check_count(Pid) when is_pid(Pid) ->
    Count = gen_server:call(Pid, #query{ term= count }),
    Query = gen_server:call(Pid, #query{ term= inspect }),
    ?assertEqual(Count, if Query#state.next - 1 < Query#state.size -> Query#state.next - 1; 
			   true -> Query#state.size end ),
    ?assertEqual(Count, dict:size(Query#state.elements)),
    ok.

windowing_test() ->
    {ok, Pid} = ?module:start_link(2),
    gen_server:call(Pid, #data{ type= int, value= 100 }),
    check_count(Pid),
    gen_server:call(Pid, #data{ type= int, value= 99 }),
    check_count(Pid),
    gen_server:call(Pid, #data{ type= int, value= 98 }),
    check_count(Pid),
    gen_server:call(Pid, #data{ type= int, value= 97 }),
    check_count(Pid),
    ok = gen_server:stop(Pid),
    ok.
