-module(exit_test).

-import(key_gen, [hash_name/1, hex_length/1]).
-import(routing, [init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/3]).
-import(leaf_set, [add_leaf/4, remove_leaf/3, closest_node/3, update_leaf_set/4]).
-import(utils, [get_time/0]).
-import(node_actions, [full_route/4, update_list/5, send_file_to_store/5, save_file_to_store/4, delete_stored_file/5,
    broadcast/3, broadcast/4, broadcast_tree/4, broadcast_tree/3, get_folder_path/1, get_file_path/2]).
-import(pastry_actions, [join_res/6, keepalive/3, backup/4, share_info/3, exit_response/6, update_keepalive/4, check_expired_nodes/5, 
  backup_res/5, suicide/3, join_res_handle/8, backup_update/5, backup_find/7, backup_found/4, new_leaf_backup/5,
  info_res/5, remove_backup_folder/2, remove_backup_file/3, old_leaf_backup/3, backup_remove/3, update_leaf_backups/5]).

-include_lib("eunit/include/eunit.hrl").
-define(CHECK_INTERVAL, 15000).
-define(KEEPALIVE_INTERVAL, 300).
-define(EXPIRATION_INTERVAL, 1000).
-define(RETRY_INTERVAL, 3000).
-define(INFO_INTERVAL, 2000).
-define (L2, 4).



exit_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    print_routing_table(RoutingTable),
    Node2 = lists:nth(1, Pids),
    Node2 ! kill_node,
    {RoutingTable1, _} = receive_exit(SelfName, RoutingTable, LeafSet),
    ?assertEqual(1, length(RoutingTable1)),
    timer:sleep(500),
    ?assert(false),
    NewPids = tl(Pids),
    cleanup(NewPids),
    cleanup_mailbox(),
    ?assert(true).

receive_exit(SelfName, RoutingTable, LeafSet) ->
    receive
        {{FromPid, FromName}, _, _Timestamp, {exit}} ->
            io:fwrite("Exit from: ~p~n", [FromName]),
            {NewRoutingTable, NewLeafSet} = exit_response({self(), SelfName}, {FromPid, FromName}, RoutingTable, LeafSet, ?L2, ?RETRY_INTERVAL),
            backup_update({self(), SelfName}, {FromPid, FromName}, NewRoutingTable, NewLeafSet, ?RETRY_INTERVAL); 
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end.
    

setup() ->
    SelfAddr = self(),
    Node2 = spawn(fun() -> node_start("node2", SelfAddr) end),
    Node3 = spawn(fun() -> node_start("node3", SelfAddr) end),
    Node4 = spawn(fun() -> node_start("node4", SelfAddr) end),
    NodeInfo1 = {self(), "node1"},
    NodeInfo2 = {Node2, "node2"},
    NodeInfo3 = {Node3, "node3"},
    NodeInfo4 = {Node4, "node4"},

    io:format("node1info: ~p~n", [NodeInfo1]),
    io:format("node2info: ~p~n", [NodeInfo2]),
    io:format("node3info: ~p~n", [NodeInfo3]),
    io:format("node4info: ~p~n", [NodeInfo4]),

    io:format("node1: ~p~n", [hash_name("node1")]),
    io:format("node2: ~p~n", [hash_name("node2")]),
    io:format("node3: ~p~n", [hash_name("node3")]),
    io:format("node4: ~p~n", [hash_name("node4")]),
    io:format("node5: ~p~n", [hash_name("node5")]),
    io:format("node6: ~p~n", [hash_name("node5")]),

    OtherNodes = [NodeInfo2, NodeInfo3],

    Routes2 = [NodeInfo3, NodeInfo4],
    Leaves2 = [NodeInfo1, NodeInfo3],
    Routes3 = [NodeInfo2, NodeInfo4],
    Leaves3 = [NodeInfo1],
    Routes4 = [NodeInfo2, NodeInfo3],
    Leaves4 = [NodeInfo1],

    Node2 ! {init, Routes2, Leaves2},
    Node3 ! {init, Routes3, Leaves3},
    Node4 ! {init, Routes4, Leaves4},

    Pids = [Node2, Node3, Node4],
    SelfName = "node1",
    RoutingTable = init_routing_table(hash_name("node1")),
    LeafSet = {[],[]},
    RoutingTable1 = lists:foldl(fun(OtherNode, RT) -> 
        add_node(OtherNode, RT)
    end, RoutingTable, OtherNodes),
    LeafSet1 = lists:foldl(fun(OtherNode, LS) -> 
        add_leaf(LS, ?L2, OtherNode, SelfName)
    end, LeafSet, OtherNodes),
    {SelfName, RoutingTable1, LeafSet1, OtherNodes, Pids}.


node_start(NodeName, FatherPid) ->
    <<Key/bitstring>> = hash_name(NodeName),
    RoutingTable = init_routing_table(Key),
    
    LeafSet = {[], []},
    node_loop(RoutingTable, LeafSet, [], {self(), NodeName}, FatherPid).


node_init({_NodePid, NodeName}, RoutingTable, LeafSet, Routes, Leaves) ->
    RoutingTable1 = lists:foldl(fun(OtherNode, RT) -> 
        add_node(OtherNode, RT)
    end, RoutingTable, Routes),
    LeafSet1 = lists:foldl(fun(OtherNode, LS) -> 
        add_leaf(LS, ?L2, OtherNode, NodeName)
    end, LeafSet, Leaves),
    {RoutingTable1, LeafSet1}.



% Main loop for the provider node
node_loop(RoutingTable, LeafSet, KeepAliveList,  SelfInfo, FatherPid) ->
  receive
     {init, Routes, Leaves} ->
        {NewRoutingTable, NewLeafSet} = node_init(SelfInfo, RoutingTable, LeafSet, Routes, Leaves),
        node_loop(NewRoutingTable, NewLeafSet, KeepAliveList,  SelfInfo, FatherPid);

    {From, _Msg_id, _Timestamp, {exit}} ->
      {NewRoutingTable, NewLeafSet} = exit_response(SelfInfo, From, RoutingTable, LeafSet, ?L2, ?RETRY_INTERVAL),
      node_loop(NewRoutingTable, NewLeafSet, KeepAliveList,  SelfInfo, FatherPid);

    kill_node ->
      io:fwrite("~p suicide", [SelfInfo]),
      suicide(SelfInfo, RoutingTable, LeafSet);

    OtherStuff ->
        io:fwrite("Trash at ~p: ~p:~n", [SelfInfo, OtherStuff]),
        node_loop(RoutingTable, LeafSet, KeepAliveList,  SelfInfo, FatherPid)
  end.


cleanup_mailbox() ->
    receive
        _ -> cleanup_mailbox()
    after 500 -> ok
    end.


cleanup(Pids) ->
    io:fwrite("Pids to kill ~p:~n", [Pids]),
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),

    ok.
