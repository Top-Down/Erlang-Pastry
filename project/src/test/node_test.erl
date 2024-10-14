-module(node_test).

-include_lib("eunit/include/eunit.hrl").

-import(node, [start_node/2, start_node/1]).
-import(utils, [get_time/0]).

start_test() ->
    % Start node1
    SelfInfo = {self(), "coord"},

    Node1 = start_node("node1"),
    Node1Info = {Node1, "node1"},
    timer:sleep(300),
    Node2 = start_node("node2", Node1Info),
    timer:sleep(300),
    Node3 = start_node("node3", Node1Info),
    timer:sleep(300),
    Node4 = start_node("node4", Node1Info),
    timer:sleep(2000),

    Pids = [Node1, Node2, Node3, Node4],

    % Call find on node1
    Node2 ! {SelfInfo, make_ref(), get_time(), {find, "node4"}},

    receive_find_response("node4"),
    cleanup(Pids).


receive_find_response(NodeExpected) ->
    receive
        {file_reply, _Timestamp, {SelfName, _}} ->
            io:format("Received find from: ~p~n", [NodeExpected]),
            ?assertEqual(NodeExpected, SelfName);
        Response -> 
            io:format("Other response: ~p~n", [Response]),
            receive_find_response(NodeExpected)
    after 5000 ->
        io:format("Test completed.~n")
    end.


cleanup(Pids) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
    ok.