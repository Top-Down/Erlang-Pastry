-module(node_test).

-include_lib("eunit/include/eunit.hrl").

-import(node, [start_node/2, start_node/3]).
-import(utils, [get_time/0]).
-import(node_actions, [receive_file_to_store/3]).

start_test() ->
    % Start node1
    SelfInfo = {self(), "coord"},

    NodeList = ["node1", "node2", "node3", "node4", "node6"],
    delete_backup_folders(NodeList),

    Node1 = start_node("node1", "node1@localhost"),
    io:fwrite("Node1: ~p~n", [Node1]),
    Node1Info = {{node1, node1@localhost} , "node1"},
    timer:sleep(100),
    Node2 = start_node("node2","node1@localhost", Node1Info),
    io:fwrite("Node2: ~p~n", [Node2]),
    timer:sleep(100),
    Node3 = start_node("node3", "node1@localhost", Node1Info),
    io:fwrite("Node3: ~p~n", [Node3]),
    timer:sleep(100),
    Node4 = start_node("node4","node1@localhost", Node1Info),
    io:fwrite("Node4: ~p~n", [Node4]),
    timer:sleep(100),
    Node6 = start_node("node6","node1@localhost", Node1Info),
    io:fwrite("Node6: ~p~n", [Node6]),
    timer:sleep(100),

    Pids = [Node1, Node2, Node3, Node4, Node6],
    _Addrs = [
        {node1, node1@localhost},
        {node2, node1@localhost}, 
        {node3, node1@localhost},
        {node4, node1@localhost},
        {node6, node1@localhost}],

    % Call find on node1
    {node2, node1@localhost} ! {SelfInfo, make_ref(), get_time(), {store_find, "node4"}},

    receive_find_response("coord", {node4, node1@localhost}),

     timer:sleep(1000),

    {node2, node1@localhost} ! kill_node,

    timer:sleep(3000),
    ?assert(false),
    cleanup(Pids).


delete_backup_folders(NodeNames) ->
    lists:foreach(fun(NodeName) ->
        BackupDir = filename:join(["./files", NodeName, "backup"]),
        case filelib:is_dir(BackupDir) of
            true ->
                delete_dir(BackupDir),
                io:format("Deleted backup folder for node ~s~n", [NodeName]);
            false ->
                io:format("No backup folder found for node ~s~n", [NodeName])
        end
    end, NodeNames).


delete_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:foreach(fun(File) ->
        FilePath = filename:join([Dir, File]),
        case filelib:is_dir(FilePath) of
            true -> delete_dir(FilePath);
            false -> file:delete(FilePath)
        end
    end, Files),
    file:del_dir(Dir).


receive_find_response(SelfName, NodeExpected) ->
    receive
        {{_FromAddr, _FromName}, _Msg_id, _Timestamp, {store_found, StoreAddr}} ->
            io:fwrite("Store found: ~p~n", [StoreAddr]),
            ?assertEqual(NodeExpected, StoreAddr);
        Response ->
            io:format("Other response: ~p~n", [Response]),
            receive_find_response(SelfName, NodeExpected)
    after 1000 ->
        io:format("Test completed.~n"),
        ?assert(false)
    end.


cleanup(Pids) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
    ok.