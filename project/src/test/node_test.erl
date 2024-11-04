-module(node_test).

-include_lib("eunit/include/eunit.hrl").

-import(node, [start_node/2, start_node/3]).
-import(utils, [get_time/0]).
-import(node_actions, [receive_file_to_store/3]).

start_test() ->
    % Start node1
    SelfInfo = {self(), "coord"},

    Node1 = start_node("node1", "node1@localhost"),
    Node1Info = {Node1, "node1"},
    timer:sleep(300),
    Node2 = start_node("node2","node1@localhost", Node1Info),
    timer:sleep(300),
    Node3 = start_node("node3", "node1@localhost", Node1Info),
    timer:sleep(300),
    Node4 = start_node("node4","node1@localhost", Node1Info),
    timer:sleep(2000),

    Pids = [Node1, Node2, Node3, Node4],
    _Addrs = [
        {node1, node1@localhost},
        {node2, node1@localhost}, 
        {node3, node1@localhost},
        {node4, node1@localhost}],

    % Call find on node1
    {node2, node1@localhost} ! {SelfInfo, make_ref(), get_time(), {find, "node4"}},

    receive_find_response("coord", {{node4, node1@localhost}, "node4"}),
    cleanup(Pids).


receive_find_response(SelfName, NodeExpected) ->
    receive
        {From, _Msg_id, _Timestamp, {file_send, FileName, Size}} ->
            receive_file_to_store({self(), SelfName}, FileName, Size),
            io:fwrite("File_reply from ~p: ~p~n", [From, FileName]),
            ?assertEqual(NodeExpected, From);
        Response ->
            io:format("Other response: ~p~n", [Response]),
            receive_find_response(SelfName, NodeExpected)
    after 3000 ->
        io:format("Test completed.~n"),
        ?assert(false)
    end.


cleanup(Pids) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
    ok.