-module(pastry_actions).
-import(routing, [init_routing_table/1, route_key/2, add_node/2, remove_node/2, get_row/3, update_routing/2, print_routing_table/1, get_all_routes/1]).
-import(key_gen, [hash_name/1, common_hex_prefix/2, hex_length/1]).
-import(utils, [get_time/0]).
-import(network, [send_message/2]).
-import(file_handler, [store_file/3, get_file_size/1, delete_file/1, list_files/1, move_file/2]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).
-import(node_actions, [full_route/4, update_list/5, broadcast/3, broadcast_tree/3, send_file_to_store/4, get_file_path/2,
    save_file_to_store/4, get_backup_folder_path/2, get_backup_path/3, get_folder_path/1]).
-export([join_res/6, keepalive/3, backup/4, share_info/3, exit_response/6, update_keepalive/4, check_expired_nodes/5, 
  backup_res/5, suicide/3, join_res_handle/8, backup_update/5, backup_find/7, backup_found/4, new_leaf_backup/5,
  info_res/5, remove_backup_folder/2, remove_backup_file/3, old_leaf_backup/3, backup_remove/3, update_leaf_backups/5]).

-include_lib("kernel/include/file.hrl").

-define(EXPIRATION, 5000).


join_res({SelfAddr, SelfName}, {FromPid, FromName}, Msg_id, RoutingTable, LeafSet, L2) ->
    Key = hash_name(FromName),
    {_, UsedKeys} = route_key(Key, RoutingTable),
    I = hex_length(UsedKeys),
    Row = get_row(hash_name(SelfName), RoutingTable, I),
    FromPid ! {{SelfAddr, SelfName}, Msg_id, get_time(), {join_res, Row, LeafSet}},

    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> route_end;
        {Pid, _} -> Pid ! {{FromPid, FromName}, Msg_id, get_time(), {join}}
    end,

    RoutingTable1 = add_node({FromPid, FromName}, RoutingTable),
    LeafSet1 = add_leaf(LeafSet, L2, {FromPid, FromName}, SelfName),
    {RoutingTable1, LeafSet1}.


join_res_handle({SelfAddr, SelfName}, From, Row, {L,R}, RoutingTable, LeafSet, L2, RetryInterval) ->
    NodesList = [V || {_, V} <- Row],
    LeavesList = L ++ R,
    AllLeaves = [S || {_, S} <- LeavesList],
    AllNodes = NodesList ++ AllLeaves,
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, [From | AllNodes], RoutingTable, LeafSet, L2),

    update_leaf_backups({SelfAddr, SelfName}, NewLeafSet, LeafSet, [From | AllNodes], RetryInterval),
    {NewRoutingTable, NewLeafSet}.


backup({SelfAddr, SelfName}, LeafSet, FileName, RetryInterval) ->
    FilePath = get_file_path(SelfName,  FileName),
    case file:read_file(FilePath) of
        {ok, FileData} ->
            {_Res, FileSize} = get_file_size(FilePath),
            broadcast({SelfAddr, SelfName}, LeafSet, {backup, FileName, FileSize, FileData});
        _ ->
            erlang:send_after(RetryInterval, self(), {retry_backup, FileName})
    end.


backup_remove(SelfInfo, LeafSet, FileName) ->
    broadcast(SelfInfo, LeafSet, {delete_backup, FileName}).


backup_res({_SelfAddr, SelfName}, {_FromAddr, FromName}, FileName, FileSize, FileData) ->
    FilePath = get_backup_path(SelfName, FromName, FileName),
    store_file(FilePath, FileSize, FileData).


backup_update({SelfAddr, SelfName}, {_FromAddr, FromName}, RoutingTable, LeafSet, RetryInterval) ->
    BackupFolder = get_backup_folder_path(SelfName, FromName),
    Files = list_files(BackupFolder),
    lists:foreach(fun(FileName) ->
        Key = hash_name(FileName),
        case full_route(SelfName, RoutingTable, LeafSet, Key) of
            route_end -> 
                DestPath = get_file_path(SelfName, FileName),
                SourcePath = get_backup_path(SelfName, FromName, FileName),
                move_file(SourcePath, DestPath),
                backup({SelfAddr, SelfName}, LeafSet, DestPath, RetryInterval);
            {Pid, _} -> 
                Pid ! {{SelfAddr, SelfName}, make_ref(), get_time(), {backup_find, FileName, FromName}}
        end
    end, Files).


backup_find({SelfAddr, SelfName}, {FromAddr, FromName}, Msg_id, RoutingTable, LeafSet, FileName, BackupName) ->
    Key = hash_name(FileName),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> 
            FromAddr ! {{SelfAddr, SelfName}, Msg_id, get_time(), {backup_found, BackupName}};
        {Pid, _} -> Pid ! {{FromAddr, FromName}, Msg_id, get_time(), {backup_find, FileName, BackupName}}
    end.


backup_found({_SelfAddr, SelfName}, {_FromAddr, FromName}, FileName, BackupName) ->
    DestPath = get_backup_path(SelfName, FromName, FileName),
    SourcePath = get_backup_path(SelfName, BackupName, FileName),
    move_file(SourcePath, DestPath).


backup_folder_to({SelfAddr, SelfName}, {FromAddr, FromName}, RetryInterval) ->
    FolderPath = get_folder_path(SelfName),
    Files = list_files(FolderPath),
    lists:foreach(fun(FileName) ->
        FilePath = get_file_path(SelfName, FileName),
        case file:read_file(FilePath) of
            {ok, FileData} ->
                FileSize = byte_size(FileData),
                FromAddr ! {{SelfAddr, SelfName}, make_ref(), get_time(), {backup, FileName, FileSize, FileData}};
            {error, _Reason} ->
                erlang:send_after(RetryInterval, self(), {retry_backup, FileName, {FromAddr, FromName}})
        end
    end, Files).


update_leaf_backups(SelfInfo, NewLeafSet, {OldL, OldR}, NewNodes, RetryInterval) ->
    lists:foreach(fun(Node) ->
        new_leaf_backup(SelfInfo, Node, NewLeafSet, {OldL, OldR}, RetryInterval)
    end, NewNodes),

    OldList = OldL ++ OldR,
    OldNodes = [Node || {_Key, Node} <- OldList],
    lists:foreach(fun(Node) ->
        old_leaf_backup(SelfInfo, Node, NewLeafSet)
    end, OldNodes).


new_leaf_backup({SelfAddr, SelfName}, {FromAddr, FromName}, {L, R}, {OldL, OldR}, RetryInterval) ->
    LeafSetNodes = L ++ R,
    OldLeafSetNodes = OldL ++ OldR,
    Leaf = {hash_name(FromName), {FromAddr, FromName}},

    case {lists:member(Leaf, LeafSetNodes), lists:member(Leaf, OldLeafSetNodes)} of
        {true, false} ->
            backup_folder_to({SelfAddr, SelfName}, {FromAddr, FromName}, RetryInterval);
        _ ->
            ok
    end.


old_leaf_backup({SelfAddr, SelfName}, {FromAddr, FromName}, {L, R}) ->
    LeafSetNodes = L ++ R,

    Leaf = {hash_name(FromName), {FromAddr, FromName}},
    case lists:member(Leaf, LeafSetNodes) of
        false ->
            FromAddr ! {{SelfAddr, SelfName}, make_ref(), get_time(), {delete_backup_folder}};
        true ->
            ok
    end.


remove_backup_folder({_SelfAddr, SelfName}, {_FromAddr, FromName}) ->
    FolderPath = get_backup_folder_path(SelfName, FromName),
    case file:list_dir(FolderPath) of
        {ok, Files} ->
            lists:foreach(fun(File) ->
                delete_file(filename:join(FolderPath, File))
            end, Files),
            file:del_dir(FolderPath);
        {error, _Reason} ->
            ok
    end.


remove_backup_file({_SelfAddr, SelfName}, {_FromAddr, FromName}, FileName) ->
    FilePath = get_backup_path(SelfName, FromName, FileName),
    delete_file(FilePath).


keepalive(SelfInfo, RoutingTable, LeafSet) ->
    broadcast_tree(SelfInfo, RoutingTable, {alive}),
    broadcast(SelfInfo, LeafSet, {alive}).


share_info(SelfInfo, DestList, {L, R}) ->
    Leaves = L ++ R,
    NodesList = [Node || {_Key, Node} <- Leaves],
    broadcast(SelfInfo, DestList, {info, NodesList}).


info_res({_SelfAddr, SelfName}, NodesList, RoutingTable, LeafSet, L2) ->
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, NodesList, RoutingTable, LeafSet, L2),
    {NewRoutingTable, NewLeafSet}.


exit_response({SelfAddr, SelfName}, From, RoutingTable, LeafSet, L2, RetryInterval) ->

    CleanLeafSet = remove_leaf(LeafSet,From, SelfName),
    NewRoutingTable = remove_node(From, RoutingTable),
    AllRoutes = get_all_routes(NewRoutingTable),
    {NewLeft, NewRight} = update_leaf_set(AllRoutes, CleanLeafSet, L2, SelfName),
    
    NewLeavesList = NewLeft ++ NewRight,
    NewLeaves = [Node || {_Key, Node} <- NewLeavesList],
    lists:foreach(fun(Node) ->
        new_leaf_backup({SelfAddr, SelfName}, Node, {NewLeft, NewRight}, CleanLeafSet, RetryInterval) 
    end, NewLeaves),
    {NewRoutingTable, {NewLeft, NewRight}}.


% Function to update the KeepaliveList with the current time for the given node
update_keepalive(From, _, Timestamp, AliveNodes) ->
    case lists:keymember(From, 1, AliveNodes) of
        true -> 
            NewAliveNodes = lists:keyreplace(From, 1, AliveNodes, {From, Timestamp}),
            NewAliveNodes;
        false -> 
            NewAliveNodes = [{From, Timestamp} | AliveNodes],
            NewAliveNodes
    end.


% Function to check for expired nodes and delete them from RoutingTable and Leafset
check_expired_nodes({_SelfAddr, SelfName}, AliveNodes, RoutingTable, LeafSet, ExpirationTime) ->
    CurrentTime = get_time(),

    ExpiredNodes = lists:filter(fun({_, Timestamp}) -> 
        CurrentTime - Timestamp >= ?EXPIRATION end, AliveNodes),
    NodesList = [Node || {Node, _Timestamp} <- ExpiredNodes],

    {NewRoutingTable, NewLeafset} = lists:foldl(fun(Node, {RT, LS}) ->
        {remove_node(Node, RT), remove_leaf(LS, Node, SelfName)}
    end, {RoutingTable, LeafSet}, NodesList),
    NewAliveNodes = lists:filter(fun({_, Timestamp}) -> 
        CurrentTime - Timestamp < ExpirationTime end, AliveNodes),
    {NewAliveNodes, NewRoutingTable, NewLeafset}.


suicide(SelfInfo, RoutingTable, LeafSet) ->
    Msg = {exit},
    broadcast(SelfInfo, LeafSet, Msg),
    broadcast_tree(SelfInfo, RoutingTable, Msg),
    exit(normal).