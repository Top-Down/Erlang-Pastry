-module(node).

-import(node_actions, [update_list/5]).
-import(web_responses, [find_store/6, store/7, find/6, delete/7, 
  get_files_res/6, get_files_res_handle/2, get_all_files/7, 
  check_expired_blacklist/2, all_files_res/4]).
-import(pastry_actions, [join_res/6, keepalive/3, backup/3, 
  share_info/3, exit_response/4, update_keepalive/4, 
  check_expired_nodes/5, backup_response/4, suicide/3, 
  join_res_handle/7]).
-import(routing, [init_routing_table/1]).
-import(key_gen, [hash_name/1]).
-import(utils, [get_time/0]).
-import(network, [start_net/3]).
-export([start_node/2, start_node/4, start_node/3]).

-define(CHECK_INTERVAL, 15000).
-define(KEEPALIVE_INTERVAL, 3000).
-define(EXPIRATION_INTERVAL, 2000).
-define(BLACKLIST_INTERVAL, 1000).
-define(FLOOD_INTERVAL, 1000).
-define(L2, 2).


start_node(Name, NodeName) ->
  spawn(fun() -> bootstrap_node(Name, NodeName) end).
start_node(Name, NodeName, Starter) ->
  spawn(fun() -> bootstrap_node(Name, NodeName, Starter) end).
start_node(Name, NodeName, Starter, LeafSet) ->
  spawn(fun() -> bootstrap_node(Name, NodeName, Starter, LeafSet) end).


bootstrap_node(Name, NodeName) ->
    bootstrap_node(Name, NodeName, undefined, {[], []}).
bootstrap_node(Name, NodeName, Starter) ->
    bootstrap_node(Name, NodeName, Starter, {[], []}).
bootstrap_node(Name, NodeName, Starter, LeafSet) ->
    {SelfInfo, RoutingTable, KeepAliveList} = init_node(Name, NodeName),
    erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
    erlang:send_after(?CHECK_INTERVAL, self(), check_nodes),
    erlang:send_after(?BLACKLIST_INTERVAL, self(), check_blacklist),
    {NewRoutingTable, NewLeafSet} = join_net(SelfInfo, Starter, RoutingTable, LeafSet, ?L2),
    node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfInfo, [], []).


init_node(Name, NodeName) ->
  % Start the node with a name and cookie
  Cookie = "pastry",
  {ok, SelfAddr} = start_net(Name, NodeName, Cookie),

  SelfName = Name,
  % Initialize routing table and keepalive list
  RoutingTable = init_routing_table(hash_name(Name)),
  KeepAliveList = [],
  {{SelfAddr, SelfName}, RoutingTable, KeepAliveList}.


join_net(_, undefined,RoutingTable, LeafSet, _) ->
  {RoutingTable, LeafSet};
join_net({SelfAddr, SelfName}, {StarterPid, StarterName}, RoutingTable, {L, R}, L2) ->
    Leaves = L++R,
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, [{StarterPid, StarterName} | Leaves], RoutingTable,{L,R}, L2),
    StarterPid ! {{SelfAddr, SelfName}, make_ref(), get_time(), {join}},
    io:format("Join sent from ~p to ~p~n", [SelfName, StarterName]),
    {NewRoutingTable, NewLeafSet}.


% Main loop for the provider node
node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo, FilesList, BlackList) ->
  receive
    {From, Msg_id, Timestamp, {find, File}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      find(SelfInfo, From, Msg_id, RoutingTable, LeafSet, File),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {From, Msg_id, Timestamp, {delete, File}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      NewFilesList = delete(SelfInfo, From, Msg_id, RoutingTable, LeafSet, File, FilesList),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, NewFilesList, BlackList);
    
    {From, Msg_id, Timestamp, {store_find, FileName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      NewFilesList = find_store(SelfInfo, From, Msg_id, RoutingTable, LeafSet, FileName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, NewFilesList, BlackList);

    {From, Msg_id, _Timestamp, {store, FileName, FileSize, FileData}} ->
      NewFilesList = store(SelfInfo, From, Msg_id, FileName, FileSize, FileData, FilesList),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo, NewFilesList, BlackList);

    {From, Msg_id, _Timestamp, {get_all_files}} ->
      {NewFilesList, NewBlackList}  = get_all_files(SelfInfo, From, Msg_id, RoutingTable, LeafSet, BlackList, ?FLOOD_INTERVAL),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo, NewFilesList, NewBlackList);

    {From, Msg_id, Timestamp, {backup, FileName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      backup_response(SelfInfo, From, Msg_id, FileName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);
    
    {From, Msg_id, Timestamp, {files_req}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      NewBlackList = get_files_res(SelfInfo, From, Msg_id, RoutingTable, LeafSet, BlackList),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, NewBlackList);

    {From, Msg_id, Timestamp, {files_res, NewFiles}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      NewFilesList = get_files_res_handle(FilesList, NewFiles),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, NewFilesList, BlackList);

    {From, Msg_id, Timestamp, {join}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_res(SelfInfo, From, Msg_id, RoutingTable, LeafSet, ?L2),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {{FromPid, FromName}, Msg_id, Timestamp, {join_res, Row, SharedLeafSet}} ->
      NewKeepAliveList = update_keepalive({FromPid, FromName}, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_res_handle(SelfInfo, {FromPid, FromName}, Row, SharedLeafSet, RoutingTable, LeafSet, ?L2),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {From, _Msg_id, _Timestamp, {exit}} ->
      {NewRoutingTable, NewLeafSet} = exit_response(SelfInfo, From, RoutingTable, LeafSet),
      node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfInfo,  FilesList, BlackList);

    {From, Msg_id, Timestamp, {alive}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo,  FilesList, BlackList);

    send_keepalive ->
      keepalive(SelfInfo, RoutingTable, LeafSet),
      erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo,  FilesList, BlackList);
    
    check_nodes ->
      {NewKeepAliveList, NewRoutingTable, NewLeafset} = check_expired_nodes(SelfInfo, KeepAliveList, RoutingTable, LeafSet, ?EXPIRATION_INTERVAL),
      erlang:send_after(?CHECK_INTERVAL, self(), check_nodes),
      node_loop(NewRoutingTable, NewLeafset, NewKeepAliveList, SelfInfo,  FilesList, BlackList);

    check_blacklist ->
      NewBlackList = check_expired_blacklist(BlackList, ?BLACKLIST_INTERVAL),
      erlang:send_after(?BLACKLIST_INTERVAL, self(), check_blacklist),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo,  FilesList, NewBlackList);

    kill_node ->
      suicide(SelfInfo, RoutingTable, LeafSet);

    {flood_end, From, Msg_Id} ->
      all_files_res(SelfInfo, From, Msg_Id, FilesList),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo,  FilesList, BlackList);
    
    _ ->
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo,  FilesList, BlackList)
  end.


