-module(pastry_actions_test).

-import(key_gen, [hash_name/1, hex_length/1]).
-import(routing, [init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/3]).
-import(leaf_set, [add_leaf/4, remove_leaf/3, closest_node/3, update_leaf_set/4]).
-import(utils, [get_time/0]).
-import(node_actions, [full_route/4, update_list/5, send_file_to_store/5, save_file_to_store/4, delete_stored_file/5,
    broadcast/3, broadcast/4, broadcast_tree/4, broadcast_tree/3, get_folder_path/1, get_file_path/2]).
-import(pastry_actions, [join_res/6, keepalive/3, backup/6, share_info/3, exit_response/4, update_keepalive/4, check_expired_nodes/5, 
  backup_response/4, suicide/3, join_res_handle/7]).

-include_lib("eunit/include/eunit.hrl").
-define(CHECK_INTERVAL, 15000).
-define(KEEPALIVE_INTERVAL, 300).
-define(EXPIRATION_INTERVAL, 1000).
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
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    Node1 = self(),
    Node5 = spawn(fun() -> node_join_init("node5", {Node1, "node1"}) end),
    io:fwrite("PID NODE 5: ~p~n", [Node5]),


    receive
        {{FromPid, FromName}, Msg_id, _Timestamp, join} ->
            {NewRoutingTable, _} = join_res({self(), SelfName}, {FromPid, FromName}, Msg_id, RoutingTable, LeafSet, ?L2),
            ?assertEqual(3, length(NewRoutingTable));
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

    cleanup([Node5 | Pids]),
    cleanup_mailbox(),
    ?assert(true).


node_join_init(SelfName, {RootPid, RootName}) ->
    RoutingTable = init_routing_table(hash_name(SelfName)),
    LeafSet = {[],[]},
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, [{RootPid, RootName}], RoutingTable, LeafSet, ?L2),
    RootPid ! {{self(), SelfName}, 0, get_time(), join},
    node_loop(NewRoutingTable, NewLeafSet, [], {self(), SelfName}).


exit_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    print_routing_table(RoutingTable),
    Node2 = lists:nth(1, Pids),
    Node2 ! kill_node,
    {RoutingTable1, _} = receive_exit(SelfName, RoutingTable, LeafSet),
    ?assertEqual(1, length(RoutingTable1)),
    NewPids = tl(Pids),
    cleanup(NewPids),
    cleanup_mailbox(),
    ?assert(true).

receive_exit(SelfName, RoutingTable, LeafSet) ->
    receive
        {{FromPid, FromName}, _, _Timestamp, {exit}} ->
            io:fwrite("Exit from: ~p~n", [FromName]),
            exit_response({self(), SelfName}, {FromPid, FromName}, RoutingTable, LeafSet);
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end.
    

setup() ->
    Node2 = spawn(fun() -> node_start("node2") end),
    Node3 = spawn(fun() -> node_start("node3") end),
    Node4 = spawn(fun() -> node_start("node4") end),
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

    OtherNodes = [NodeInfo2, NodeInfo3],

    Routes2 = [NodeInfo3, NodeInfo4],
    Leaves2 = [NodeInfo1],
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


node_start(NodeName) ->
    <<Key/bitstring>> = hash_name(NodeName),
    RoutingTable = init_routing_table(Key),
    
    LeafSet = {[], []},
    node_loop(RoutingTable, LeafSet, [], {self(), NodeName}).


node_init({_NodePid, NodeName}, RoutingTable, LeafSet, Routes, Leaves) ->
    RoutingTable1 = lists:foldl(fun(OtherNode, RT) -> 
        add_node(OtherNode, RT)
    end, RoutingTable, Routes),
    LeafSet1 = lists:foldl(fun(OtherNode, LS) -> 
        add_leaf(LS, ?L2, OtherNode, NodeName)
    end, LeafSet, Leaves),
    {RoutingTable1, LeafSet1}.



% Main loop for the provider node
node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo) ->
  receive
     {init, Routes, Leaves} ->
        {NewRoutingTable, NewLeafSet} = node_init(SelfInfo, RoutingTable, LeafSet, Routes, Leaves),
        node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfInfo);


    {From, Msg_id, Timestamp, {join}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_res(SelfInfo, From, Msg_id, RoutingTable, LeafSet, ?L2),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfInfo);

    {{FromPid, FromName}, Msg_id, Timestamp, {join_res, Row, SharedLeafSet}} ->
      NewKeepAliveList = update_keepalive({FromPid, FromName}, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_res_handle(SelfInfo, {FromPid, FromName}, Row, SharedLeafSet, RoutingTable, LeafSet, ?L2),
      FromPid ! {SelfInfo, 7, join_end, NewRoutingTable},
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfInfo);

    {From, _Msg_id, _Timestamp, {exit}} ->
      {NewRoutingTable, NewLeafSet} = exit_response(SelfInfo, From, RoutingTable, LeafSet),
      node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfInfo);

    {From, Msg_id, Timestamp, {alive}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo);

    start_keepalive ->
        erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
        node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo);

    send_keepalive ->
      keepalive(SelfInfo, RoutingTable, LeafSet),
      erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo);
    
    check_nodes ->
      {NewKeepAliveList, NewRoutingTable, NewLeafset} = check_expired_nodes(SelfInfo, KeepAliveList, RoutingTable, LeafSet, ?EXPIRATION_INTERVAL),
      erlang:send_after(?CHECK_INTERVAL, self(), check_nodes),
      node_loop(NewRoutingTable, NewLeafset, NewKeepAliveList, SelfInfo);

    kill_node ->
      suicide(SelfInfo, RoutingTable, LeafSet);

    _ ->
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo)
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
