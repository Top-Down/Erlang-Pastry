-module(node).

-import(node_actions, [update_list/5]).
-import(web_responses, [find_store/6, store/7, find/6, delete/7, 
  get_files_res/6, get_files_res_handle/2, get_all_files/7, 
  check_expired_blacklist/2, all_files_res/4]).
-import(pastry_actions, [join_res/6, keepalive/3, share_info/3, 
    exit_response/5, update_keepalive/4, check_expired_nodes/5, 
    suicide/3, join_res_handle/7, info_res/5, send_alive_res/3,
    refresh_file_owner/3]).
-import(backup_actions, [backup/3, backup_res/5, backup_update/4, 
    backup_find/7, backup_found/4, new_leaf_backup/4,
    remove_backup_folder/2, remove_backup_file/3, old_leaf_backup/4, 
    backup_remove/3, update_leaf_backups/4]).
-import(routing, [init_routing_table/1]).
-import(key_gen, [hash_name/1]).
-import(utils, [get_time/0]).
-import(network, [start_net/3]).
-export([start_node/2, start_node/4, start_node/3]).

-define(CHECK_INTERVAL, 4000).
-define(KEEPALIVE_INTERVAL, 1000).
-define(EXPIRATION_INTERVAL, 3000).
-define(BLACKLIST_INTERVAL, 1000).
-define(FLOOD_INTERVAL, 1000).
-define(INFO_INTERVAL, 1000).
-define(L2, 8).


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
    erlang:send_after(?INFO_INTERVAL, self(), share_info),
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
    NodesList = [Node || {_Key, Node} <- Leaves],
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, [{StarterPid, StarterName} | NodesList], RoutingTable,{L,R}, L2),
    StarterPid ! {{SelfAddr, SelfName}, make_ref(), get_time(), {join}},
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
      backup_remove(SelfInfo, From, File),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, NewFilesList, BlackList);
    
    {From, Msg_id, Timestamp, {store_find, FileName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      NewFilesList = find_store(SelfInfo, From, Msg_id, RoutingTable, LeafSet, FileName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, NewFilesList, BlackList);

    {From, Msg_id, _Timestamp, {store, FileName, FileSize, FileData}} ->
      NewFilesList = store(SelfInfo, From, Msg_id, FileName, FileSize, FileData, FilesList),
      backup(SelfInfo, LeafSet, FileName),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo, NewFilesList, BlackList);

    {From, Msg_id, _Timestamp, {get_all_files}} ->
      {NewFilesList, NewBlackList}  = get_all_files(SelfInfo, From, Msg_id, RoutingTable, LeafSet, BlackList, ?FLOOD_INTERVAL),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo, NewFilesList, NewBlackList);
    
    {From, Msg_id, Timestamp, {files_req}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      NewBlackList = get_files_res(SelfInfo, From, Msg_id, RoutingTable, LeafSet, BlackList),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, NewBlackList);

    {From, Msg_id, Timestamp, {files_res, NewFiles}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      NewFilesList = get_files_res_handle(FilesList, NewFiles),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, NewFilesList, BlackList);

    {From, Msg_id, Timestamp, {backup, FileName, FileSize, FileData}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      backup_res(SelfInfo, From, FileName, FileSize, FileData),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {From, Msg_id, Timestamp, {join}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_res(SelfInfo, From, Msg_id, RoutingTable, LeafSet, ?L2),
      refresh_file_owner(SelfInfo, RoutingTable, LeafSet),
      update_leaf_backups(SelfInfo, NewLeafSet, LeafSet, [From]),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {{FromPid, FromName}, Msg_id, Timestamp, {join_res, Row, SharedLeafSet}} ->
      NewKeepAliveList = update_keepalive({FromPid, FromName}, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_res_handle(SelfInfo, {FromPid, FromName}, Row, SharedLeafSet, RoutingTable, LeafSet, ?L2),
      refresh_file_owner(SelfInfo, RoutingTable, LeafSet),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);
    
    {From, Msg_id, Timestamp, {backup_find, FileName, BackupName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      backup_find(SelfInfo, From, Msg_id, RoutingTable, LeafSet, FileName, BackupName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {From, Msg_id, Timestamp, {backup_found, FileName, BackupName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      backup_found(SelfInfo, From, FileName, BackupName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {From, Msg_id, Timestamp, {delete_backup_folder}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      remove_backup_folder(SelfInfo, From),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {From, Msg_id, Timestamp, {delete_backup, FileName}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      remove_backup_file(SelfInfo, From, FileName),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo, FilesList, BlackList);

    {From, _Msg_id, _Timestamp, {exit}} ->
      {NewRoutingTable, NewLeafSet} = exit_response(SelfInfo, From, RoutingTable, LeafSet, ?L2),
      backup_update(SelfInfo, From, NewRoutingTable, NewLeafSet),
      node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfInfo,  FilesList, BlackList);

    {From, Msg_id, Timestamp, {alive}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      send_alive_res(SelfInfo, From, Msg_id),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo,  FilesList, BlackList);

    {From, Msg_id, Timestamp, {alive_res}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfInfo,  FilesList, BlackList);

    {From, Msg_id, Timestamp, {info, NodesList}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = info_res(SelfInfo, NodesList, RoutingTable, LeafSet, ?L2),
      refresh_file_owner(SelfInfo, RoutingTable, LeafSet),
      update_leaf_backups(SelfInfo, NewLeafSet, LeafSet, NodesList),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfInfo,  FilesList, BlackList);

    share_info ->
      share_info(SelfInfo, LeafSet, LeafSet),
      erlang:send_after(?INFO_INTERVAL, self(), share_info),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo,  FilesList, BlackList);

    send_keepalive ->
      keepalive(SelfInfo, RoutingTable, LeafSet),
      erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfInfo,  FilesList, BlackList);
    
    check_nodes ->
      {NewKeepAliveList, NewRoutingTable, NewLeafset} = check_expired_nodes(SelfInfo, KeepAliveList, RoutingTable, LeafSet, ?EXPIRATION_INTERVAL),
      NodesList = [Node || {Node, _Timestamp} <- NewKeepAliveList, not lists:keymember(Node, 1, KeepAliveList)],
      update_leaf_backups(SelfInfo, NewLeafset, LeafSet, NodesList),
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


