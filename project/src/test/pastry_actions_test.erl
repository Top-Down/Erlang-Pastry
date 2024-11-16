-module(pastry_actions_test).

-import(key_gen, [hash_name/1, hex_length/1]).
-import(routing, [init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/3]).
-import(leaf_set, [add_leaf/4, remove_leaf/3, closest_node/3, update_leaf_set/4]).
-import(utils, [get_time/0]).
-import(node_actions, [full_route/4, update_list/5, broadcast/4, broadcast_leaf/4, broadcast_leaf/3, 
    broadcast_routing/4, broadcast_routing/3, send_file_to_store/4, get_file_path/2,
    save_file_to_store/4, get_backup_folder_path/2, get_backup_path/3, get_folder_path/1]).
-import(backup_actions, [backup/3, backup_res/5, backup_update/4, backup_find/7, backup_found/4, new_leaf_backup/4,
    remove_backup_folder/2, remove_backup_file/3, old_leaf_backup/3, backup_remove/3, update_leaf_backups/4]).
-import(pastry_actions, [join_res/6, keepalive/3, share_info/3, exit_response/5, update_keepalive/4, check_expired_nodes/5, 
    suicide/3, join_res_handle/7, info_res/5, send_alive_res/3]).

-include_lib("eunit/include/eunit.hrl").
-define(CHECK_INTERVAL, 15000).
-define(KEEPALIVE_INTERVAL, 300).
-define(EXPIRATION_INTERVAL, 1000).
-define(INFO_INTERVAL, 2000).
-define (L2, 4).


keepalive_test() ->
    {_, _, _, _, Pids} = setup(),
    KeepAliveList = [],

    Pid2 = lists:nth(1, Pids),
    Pid2 ! start_keepalive,
    NewAliveNodes = receive_keepalive(KeepAliveList),
    ?assertEqual(1, length(NewAliveNodes)),

    io:format("NewAliveNodes: ~p~n", [NewAliveNodes]),

    Pid3 = lists:nth(2, Pids),
    Pid3 ! start_keepalive,

    NewAliveNodes2 = receive_keepalive(NewAliveNodes),
    io:format("NewAliveNodes2: ~p~n", [NewAliveNodes2]),
    ?assertEqual(1, length(NewAliveNodes2)),

    NewAliveNodes3 = receive_keepalive(NewAliveNodes2),
    io:format("NewAliveNodes3: ~p~n", [NewAliveNodes3]),
    ?assertEqual(2, length(NewAliveNodes3)),

    cleanup(Pids),
    cleanup_mailbox(),
    ?assert(true).



check_alive_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    KeepAliveList = [],

    Pid2 = lists:nth(1, Pids),
    Pid2 ! start_keepalive,
    NewAliveNodes = receive_keepalive(KeepAliveList),
    ?assertEqual(1, length(NewAliveNodes)),

    io:format("NewAliveNodes: ~p~n", [NewAliveNodes]), 

    timer:sleep(2000),
    {NewAliveNodes1, _, _} = check_expired_nodes({self(), SelfName}, NewAliveNodes, RoutingTable, LeafSet, ?EXPIRATION_INTERVAL),

    ?assertEqual(0, length(NewAliveNodes1)),
    cleanup(Pids),
    cleanup_mailbox(),
    ?assert(true).


receive_keepalive(KeepAliveList) ->
    receive
        {From, Msg_id, Timestamp, {alive}} -> 
            update_keepalive(From, Msg_id, Timestamp, KeepAliveList);
        Other2 -> 
            io:fwrite("Unexpected file message: ~p~n", [Other2]),
            error(unexpected_message),
            ?assert(false)
    after 3000 ->
        ?assert(false),
        error(timeout)
    end.


join_test() ->
    {SelfName, RoutingTableOld, LeafSet, _, Pids} = setup(),
    Node1 = self(),
    Node4 = {lists:nth(3, Pids), "node4"},

    RoutingTable = add_node(Node4, RoutingTableOld),

    Node5 = spawn(fun() -> node_join_init("node6", {Node1, "node1"}) end),
    io:fwrite("PID NODE 6: ~p~n", [Node5]),

    io:fwrite("Leafset node1 ~p:~n", [LeafSet]),

    receive
        {{FromPid, FromName}, Msg_id, _Timestamp, {join}} ->
            {{_, _NewRoutingTable}, _} = join_res({self(), SelfName}, {FromPid, FromName}, Msg_id, RoutingTable, LeafSet, ?L2);
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,

    receive
        {_, _, join_end, JoinTable} ->
            io:fwrite("Join end: ~p~n", [JoinTable]);
        Other1 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other1]),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,

    receive
        {_, _, join_end, JoinTable1} ->
            io:fwrite("Join end: ~p~n", [JoinTable1]);
        Other2 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other2]),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,

    timer:sleep(1000),
    cleanup([Node5 | Pids]),
    cleanup_mailbox().


node_join_init(SelfName, {RootPid, RootName}) ->
    RoutingTable = init_routing_table(hash_name(SelfName)),
    LeafSet = {[],[]},
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, [{RootPid, RootName}], RoutingTable, LeafSet, ?L2),
    RootPid ! {{self(), SelfName}, 0, get_time(), {join}},
    node_loop(NewRoutingTable, NewLeafSet, [], {self(), SelfName}, RootPid).


exit_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    Node2 = lists:nth(1, Pids),
    Node2 ! kill_node,
    {{_, RoutingTable1}, _} = receive_exit(SelfName, RoutingTable, LeafSet),
    ?assertEqual(0, length(lists:nth(1, RoutingTable1))),
    timer:sleep(500),
    NewPids = tl(Pids),
    cleanup(NewPids),
    cleanup_mailbox(),
    ?assert(true).

receive_exit(SelfName, RoutingTable, LeafSet) ->
    receive
        {{FromPid, FromName}, _, _Timestamp, {exit}} ->
            io:fwrite("Exit from: ~p~n", [FromName]),
            {NewRoutingTable, NewLeafSet} = exit_response({self(), SelfName}, {FromPid, FromName}, RoutingTable, LeafSet, ?L2 ),
            backup_update({self(), SelfName}, {FromPid, FromName}, NewRoutingTable, NewLeafSet ),
            {NewRoutingTable, NewLeafSet};
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

    
    {From, Msg_id, Timestamp, {backup, FileName, FileSize, FileData}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      backup_res(SelfInfo, From, FileName, FileSize, FileData),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList,  SelfInfo, FatherPid);
    
    {From, Msg_id, Timestamp, {join}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_res(SelfInfo, From, Msg_id, RoutingTable, LeafSet, ?L2),
      io:fwrite("~p - Leafset after join ~p:~n", [SelfInfo, NewLeafSet]),
      update_leaf_backups(SelfInfo, NewLeafSet, LeafSet, [From] ),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList,  SelfInfo, FatherPid);

    {{FromPid, FromName}, Msg_id, Timestamp, {join_res, Row, SharedLeafSet}} ->
      NewKeepAliveList = update_keepalive({FromPid, FromName}, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_res_handle(SelfInfo, {FromPid, FromName}, Row, SharedLeafSet, RoutingTable, LeafSet, ?L2),
      io:fwrite("~p - Leafset after join res ~p:~n", [SelfInfo, NewLeafSet]),
      FatherPid ! {SelfInfo, 0, join_end, FromName},
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfInfo, FatherPid);
    
    {From, Msg_id, Timestamp, {backup_find, FileName, BackupName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      backup_find(SelfInfo, From, Msg_id, RoutingTable, LeafSet, FileName, BackupName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList,  SelfInfo, FatherPid);

    {From, Msg_id, Timestamp, {backup_found, FileName, BackupName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      backup_found(SelfInfo, From, FileName, BackupName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList,  SelfInfo, FatherPid);

    {From, Msg_id, Timestamp, {delete_backup_folder}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      remove_backup_folder(SelfInfo, From),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList,  SelfInfo, FatherPid);

    {From, Msg_id, Timestamp, {delete_backup, FileName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      remove_backup_file(SelfInfo, From, FileName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList,  SelfInfo, FatherPid);

    {From, _Msg_id, _Timestamp, {exit}} ->
      {NewRoutingTable, NewLeafSet} = exit_response(SelfInfo, From, RoutingTable, LeafSet, ?L2 ),
      backup_update(SelfInfo, From, RoutingTable, LeafSet ),
      node_loop(NewRoutingTable, NewLeafSet, KeepAliveList,  SelfInfo, FatherPid);

    {From, Msg_id, Timestamp, {alive}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList,  SelfInfo, FatherPid);

    {From, Msg_id, Timestamp, {info, NodesList}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = info_res(SelfInfo, NodesList, RoutingTable, LeafSet, ?L2),
      update_leaf_backups(SelfInfo, NewLeafSet, LeafSet, NodesList ),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList,  SelfInfo, FatherPid);

    start_keepalive ->
        erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
        node_loop(RoutingTable, LeafSet, KeepAliveList,  SelfInfo, FatherPid);

    start_info ->
        erlang:send_after(?INFO_INTERVAL, self(), share_info),
        node_loop(RoutingTable, LeafSet, KeepAliveList,  SelfInfo, FatherPid);

    share_info ->
      share_info(SelfInfo, LeafSet, LeafSet),
      erlang:send_after(?INFO_INTERVAL, self(), share_info),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo, FatherPid);

    send_keepalive ->
      keepalive(SelfInfo, RoutingTable, LeafSet),
      erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
      node_loop(RoutingTable, LeafSet, KeepAliveList,  SelfInfo, FatherPid);
    
    check_nodes ->
      {NewKeepAliveList, NewRoutingTable, NewLeafset} = check_expired_nodes(SelfInfo, KeepAliveList, RoutingTable, LeafSet, ?EXPIRATION_INTERVAL),
      erlang:send_after(?CHECK_INTERVAL, self(), check_nodes),
      node_loop(NewRoutingTable, NewLeafset, NewKeepAliveList,  SelfInfo, FatherPid);

    kill_node ->
      io:fwrite("~p suicide", [SelfInfo]),
      io:fwrite("~p - Leafset ~p:~n", [SelfInfo, LeafSet]),
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
