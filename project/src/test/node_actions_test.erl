-module(node_actions_test).

-import(key_gen, [hash_name/1, hex_length/1]).
-import(routing, [init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/3]).
-import(leaf_set, [add_leaf/4, remove_leaf/3, closest_node/3, update_leaf_set/4]).
-import(utils, [get_time/0]).
-import(node_actions, [join_response/6, keepalive/3, backup/3, share_info/3, store_file/1, 
  store_response/6, exit_response/4, update_keepalive/4, check_expired_nodes/5, 
  backup_response/2, find/6, delete/6, suicide/3, full_route/4, join_update/7, update_list/5]).

-include_lib("eunit/include/eunit.hrl").
-define(CHECK_INTERVAL, 15000).
-define(KEEPALIVE_INTERVAL, 1000).
-define(EXPIRATION_INTERVAL, 1500).
-define (L2, 4).


find_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    print_routing_table(RoutingTable),
    find(SelfName, {self(), SelfName}, 1, RoutingTable, LeafSet, "node4"),
    receive
        {find_recv, Name} ->
            io:fwrite("Find_recv: ~p~n", [Name]),
            ?assertEqual("node2", Name);
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            ?assert(false),
            error(unexpected_message)
    after 5000 ->
        error(timeout)
    end,

    receive
        {find_recv, Name1} ->
            io:fwrite("Find_recv: ~p~n", [Name1]),
            ?assertEqual("node4", Name1);
        Other1 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other1]),
            ?assert(false),
            error(unexpected_message)
    after 5000 ->
        error(timeout)
    end,
    
    receive
        {file_reply, _Timestamp, {From, File}} -> 
            io:fwrite("File_reply from ~p: ~p~n", [From, File]),
            ?assertEqual("node4", File);
        Other2 -> 
            io:fwrite("Unexpected file message: ~p~n", [Other2]),
            ?assert(false),
            error(unexpected_message)
    after 5000 ->
        error(timeout)
    end,
    cleanup(Pids).


full_route_test() ->
    {SelfName, RoutingTable, LeafSet, Nodes, Pids} = setup(),
    print_routing_table(RoutingTable),
    Result = full_route(SelfName, RoutingTable, LeafSet, hash_name("node1")),
    ?assertEqual(route_end, Result),
    Result2 = full_route(SelfName, RoutingTable, LeafSet, hash_name("node2")),
    ?assertEqual(lists:nth(1, Nodes), Result2),
    cleanup(Pids).


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

    ?assertEqual(2, length(NewAliveNodes2)),
    cleanup(Pids).


check_alive_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    KeepAliveList = [],

    Pid2 = lists:nth(1, Pids),
    Pid2 ! start_keepalive,
    NewAliveNodes = receive_keepalive(KeepAliveList),
    ?assertEqual(1, length(NewAliveNodes)),

    io:format("NewAliveNodes: ~p~n", [NewAliveNodes]), 

    timer:sleep(2000),
    {NewAliveNodes1, _, _} = check_expired_nodes(SelfName, NewAliveNodes, RoutingTable, LeafSet, ?EXPIRATION_INTERVAL),

    ?assertEqual(0, length(NewAliveNodes1)),
    cleanup(Pids).


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


exit_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    print_routing_table(RoutingTable),
    Node2 = lists:nth(1, Pids),
    Node2 ! kill_node,
    {RoutingTable1, _} = receive_exit(SelfName, RoutingTable, LeafSet),
    ?assertEqual(1, length(RoutingTable1)),
    NewPids =tl(Pids),
    cleanup(NewPids).

receive_exit(SelfName, RoutingTable, LeafSet) ->
    receive
        {{FromPid, FromName}, _, _Timestamp, {exit}} ->
            io:fwrite("Exit from: ~p~n", [FromName]),
            exit_response(SelfName, {FromPid, FromName}, RoutingTable, LeafSet);
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end.



join_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    Node1 = self(),
    Node5 = spawn(fun() -> node_join_init("node5", {Node1, "node1"}) end),
    io:fwrite("PID NODE 5: ~p~n", [Node5]),


    receive
        {{FromPid, FromName}, Msg_id, _Timestamp, join} ->
            {NewRoutingTable, _} = join_response(SelfName, {FromPid, FromName}, Msg_id, RoutingTable, LeafSet, ?L2),
            ?assertEqual(3, length(NewRoutingTable));
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            error(unexpected_message)
    after 5000 ->
        error(timeout)
    end,

    receive
        {_, _, join_end, JoinTable} ->
            io:fwrite("Unexpected find message: ~p~n", [JoinTable]);
        Other1 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other1]),
            error(unexpected_message)
    after 5000 ->
        error(timeout)
    end,

    cleanup([Node5 | Pids]).


node_join_init(SelfName, {RootPid, RootName}) ->
    RoutingTable = init_routing_table(hash_name(SelfName)),
    LeafSet = {[],[]},
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, [{RootPid, RootName}], RoutingTable, LeafSet, ?L2),
    RootPid ! {{self(), SelfName}, 0, get_time(), join},
    node_loop(NewRoutingTable, NewLeafSet, [], SelfName).
    

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
    node_loop(RoutingTable, LeafSet, [], NodeName).


node_init(NodeName, RoutingTable, LeafSet, Routes, Leaves) ->
    RoutingTable1 = lists:foldl(fun(OtherNode, RT) -> 
        add_node(OtherNode, RT)
    end, RoutingTable, Routes),
    LeafSet1 = lists:foldl(fun(OtherNode, LS) -> 
        add_leaf(LS, ?L2, OtherNode, NodeName)
    end, LeafSet, Leaves),
    {RoutingTable1, LeafSet1}.


node_loop(RoutingTable, LeafSet, KeepAliveList, SelfName) ->
  receive
    {{FromPid, FromName}, Msg_id, _Timestamp, {find, File}} ->
        FromPid ! {find_recv, SelfName},
        find(SelfName, {FromPid, FromName}, Msg_id, RoutingTable, LeafSet, File),
        node_loop(RoutingTable, LeafSet, KeepAliveList, SelfName);
    {From, Msg_id, _Timestamp, join} ->
        {NewRoutingTable, NewLeafSet} = join_response(SelfName, From, Msg_id, RoutingTable, LeafSet, ?L2),
        node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfName);
    {init, Routes, Leaves} ->
        {NewRoutingTable, NewLeafSet} = node_init(SelfName, RoutingTable, LeafSet, Routes, Leaves),
        node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfName);
    {{FromPid, FromName}, _, _Timestamp, {join_res, Row, SharedLeafSet}} ->
        {NewRoutingTable, NewLeafSet} = join_update(SelfName, {FromPid, FromName}, Row, SharedLeafSet, RoutingTable, LeafSet, ?L2),
        FromPid ! {{self(), SelfName}, 0, join_end, NewRoutingTable},
        node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfName);
    start_keepalive ->
        erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
        node_loop(RoutingTable, LeafSet, KeepAliveList, SelfName);
    send_keepalive ->
        print_routing_table(RoutingTable),
        keepalive(SelfName, RoutingTable, LeafSet),
        node_loop(RoutingTable, LeafSet, KeepAliveList, SelfName);
    kill_node ->
      suicide(SelfName, RoutingTable, LeafSet);
    _ ->
        node_loop(RoutingTable, LeafSet, KeepAliveList, SelfName)
  end.

cleanup(Pids) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
    ok.
