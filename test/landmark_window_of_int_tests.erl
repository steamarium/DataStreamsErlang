-module(landmark_window_of_int_tests).
-include_lib("eunit/include/eunit.hrl").

searver_test() ->
    {ok, Pid} = landmark_window_of_int:start_link(),
    SyncCall = gen_server:call(Pid, any),
    ?assertEqual(ok, SyncCall),
    Terminated = gen_server:stop(Pid),
    ?assertEqual(ok, Terminated),
    ok.
