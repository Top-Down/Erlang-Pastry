-module(web_responses_test).

-import(key_gen, [hash_name/1, hex_length/1]).
-import(routing, [init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/3]).
-import(leaf_set, [add_leaf/4, remove_leaf/3, closest_node/3, update_leaf_set/4]).
-import(utils, [get_time/0]).
-import(node_actions, [save_file_to_store/4,send_file_to_store/5]).
-import(web_responses, [find_store/6, store/7, find/6, delete/7, get_files_res/6, get_files_res_handle/2, 
    get_all_files/7, check_expired_blacklist/2, all_files_res/4]).

-include_lib("eunit/include/eunit.hrl").
-define(CHECK_INTERVAL, 15000).
-define(BLACKLIST_INTERVAL, 3000).
-define(FLOOD_INTERVAL, 1000).
-define (L2, 4).


find_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    %print_routing_table(RoutingTable),
    find({self(), SelfName}, {self(), SelfName}, 1, RoutingTable, LeafSet, "node4"),
    receive
        {find_recv, {_, Name}} ->
            io:fwrite("Find_recv: ~p~n", [Name]),
            ?assertEqual("node2", Name);
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,

    receive
        {find_recv, {_, Name1}} ->
            io:fwrite("Find_recv: ~p~n", [Name1]),
            ?assertEqual("node4", Name1);
        Other1 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other1]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,

    receive
        {_From, _Msg_Id, _Timestamp, {find_end, FileName, FileSize, FileData}} ->
            Result = save_file_to_store({ self(), SelfName}, FileName, FileSize, FileData),
            ?assertEqual(ok, Result);
        Other2 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other2]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,
    cleanup(Pids).



delete_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    %print_routing_table(RoutingTable),
    delete({self(), SelfName}, {self(), SelfName}, 2, RoutingTable, LeafSet, "node4", []) ,
    receive
        {delete_recv, {_, Name}} ->
            io:fwrite("Find_recv: ~p~n", [Name]),
            ?assertEqual("node2", Name);
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,

    receive
        {delete_recv, {_, Name1}} ->
            io:fwrite("Find_recv: ~p~n", [Name1]),
            ?assertEqual("node4", Name1);
        Other1 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other1]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,

    receive
        {{_FromAddr, FromName}, _Msg_id, _Timestamp, {delete_end, _FileName}} ->
            ?assertEqual(FromName, "node4");
        Other2 -> 
            io:fwrite("Unexpected delete message: ~p~n", [Other2]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout)
    end,
    cleanup(Pids).

    


store_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    %print_routing_table(RoutingTable),

    find_store({self(), SelfName}, {self(), SelfName}, 3, RoutingTable, LeafSet, "node4"),
    receive
        {{_FromAddr, FromName}, Msg_id, _Timestamp, {store_found, StoreAddr}} ->
            io:fwrite("Store found: ~p~n", [StoreAddr]),
            send_file_to_store({self(), SelfName}, {StoreAddr, FromName}, store, Msg_id, "node4");
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout_store_find)
    end,
    
    receive
        {{_FromAddr1, FromName1}, _Msg_id1, _Timestamp1, {store_end}} ->
            io:fwrite("Find_recv: ~p~n", [FromName1]),
            ?assertEqual("node4", FromName1);
        Other1 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other1]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout_store_confirm)
    end,
    cleanup(Pids).




get_all_files_test() ->
    {SelfName, RoutingTable, LeafSet, _, Pids} = setup(),
    %print_routing_table(RoutingTable),

    {FilesList, BlackList} = get_all_files({self(), SelfName}, {self(), SelfName}, 4, RoutingTable, LeafSet, [], 1000),

    get_all_files_loop({self(), SelfName}, RoutingTable, LeafSet, FilesList, BlackList),

    receive
        {SelfInfo, _Msg_Id, _Timestamp, {all_files_res, FilesList}} ->
            io:fwrite("Allfilesrecv: ~p~n", [SelfInfo]),
            ?assertEqual(5, length(FilesList));
        Other1 -> 
            io:fwrite("Unexpected find message: ~p~n", [Other1])
    after 3000 ->
        error(timeout_store_confirm)
    end,
    cleanup(Pids).


get_all_files_loop(SelfInfo, RoutingTable, LeafSet, FilesList, BlackList) ->
    receive
        {From, Msg_id, _Timestamp, {files_req}} ->
            NewBlackList = get_files_res(SelfInfo, From, Msg_id, RoutingTable, LeafSet, BlackList),
            get_all_files_loop(SelfInfo, RoutingTable, LeafSet, FilesList, NewBlackList);

        {_From, _Msg_id, _Timestamp, {files_res, NewFiles}} ->
            NewFilesList = get_files_res_handle(FilesList, NewFiles),
            get_all_files_loop(SelfInfo, RoutingTable, LeafSet, NewFilesList, BlackList);

        {flood_end, From, Msg_Id} ->
            all_files_res(SelfInfo, From, Msg_Id, FilesList);
        
        Other -> 
            io:fwrite("Unexpected find message: ~p~n", [Other]),
            ?assert(false),
            error(unexpected_message)
    after 3000 ->
        error(timeout_store_find)
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
    node_loop(RoutingTable, LeafSet,{self(), NodeName}, [], []).



node_init({_NodePid, NodeName}, RoutingTable, LeafSet, Routes, Leaves) ->
    RoutingTable1 = lists:foldl(fun(OtherNode, RT) -> 
        add_node(OtherNode, RT)
    end, RoutingTable, Routes),
    LeafSet1 = lists:foldl(fun(OtherNode, LS) -> 
        add_leaf(LS, ?L2, OtherNode, NodeName)
    end, LeafSet, Leaves),
    {RoutingTable1, LeafSet1}.



node_loop(RoutingTable, LeafSet, SelfInfo, FilesList, BlackList) ->
  receive
    {init, Routes, Leaves} ->
        {NewRoutingTable, NewLeafSet} = node_init(SelfInfo, RoutingTable, LeafSet, Routes, Leaves),
        node_loop(NewRoutingTable, NewLeafSet, SelfInfo, FilesList, BlackList);


    {{FromAddr, FromName}, Msg_id, _Timestamp, {find, File}} ->
      FromAddr ! {find_recv, SelfInfo},
      find(SelfInfo, {FromAddr, FromName}, Msg_id, RoutingTable, LeafSet, File),
      node_loop(RoutingTable, LeafSet,  SelfInfo, FilesList, BlackList);

    {{FromAddr, FromName}, Msg_id, _Timestamp, {delete, File}} ->
      FromAddr ! {delete_recv, SelfInfo},
      NewFilesList = delete(SelfInfo, {FromAddr, FromName}, Msg_id, RoutingTable, LeafSet, File, FilesList),
      node_loop(RoutingTable, LeafSet,  SelfInfo, NewFilesList, BlackList);
    
    {From, Msg_id, _Timestamp, {store_find, FileName}} ->
      NewFilesList = find_store(SelfInfo, From, Msg_id, RoutingTable, LeafSet, FileName),
      node_loop(RoutingTable, LeafSet,  SelfInfo, NewFilesList, BlackList);

    {From, Msg_id, _Timestamp, {store, FileName, FileSize, FileData}} ->
      NewFilesList = store(SelfInfo, From, Msg_id, FileName, FileSize, FileData, FilesList),
      node_loop(RoutingTable, LeafSet,  SelfInfo, NewFilesList, BlackList);

    {From, Msg_id, _Timestamp, {get_all_files}} ->
      {NewFilesList, NewBlackList}  = get_all_files(SelfInfo, From, Msg_id, RoutingTable, LeafSet, BlackList, ?FLOOD_INTERVAL),
      node_loop(RoutingTable, LeafSet,  SelfInfo, NewFilesList, NewBlackList);
    
    {From, Msg_id, _Timestamp, {files_req}} ->
      NewBlackList = get_files_res(SelfInfo, From, Msg_id, RoutingTable, LeafSet, BlackList),
      node_loop(RoutingTable, LeafSet,  SelfInfo, FilesList, NewBlackList);

    {_From, _Msg_id, _Timestamp, {files_res, NewFiles}} ->
      NewFilesList = get_files_res_handle(FilesList, NewFiles),
      node_loop(RoutingTable, LeafSet,  SelfInfo, NewFilesList, BlackList);

    check_blacklist ->
      NewBlackList = check_expired_blacklist(BlackList, ?BLACKLIST_INTERVAL),
      erlang:send_after(?BLACKLIST_INTERVAL, self(), check_blacklist),
      node_loop(RoutingTable, LeafSet,  SelfInfo,  FilesList, NewBlackList);

    {flood_end, From, Msg_Id} ->
      all_files_res(SelfInfo, From, Msg_Id, FilesList),
      node_loop(RoutingTable, LeafSet,  SelfInfo,  FilesList, BlackList);
    
    UnexpectedMsg ->
      io:fwrite("Unexpected msg at node ~p: ~p~n", [SelfInfo, UnexpectedMsg]),
      node_loop(RoutingTable, LeafSet,  SelfInfo,  FilesList, BlackList)
  end.




cleanup(Pids) ->
    io:fwrite("Pids to kill ~p:~n", [Pids]),
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),

    ok.
