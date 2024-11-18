-module(backup_actions).
-import(routing, [init_routing_table/1, route_key/2, add_node/2, remove_node/2, get_row/3, update_routing/2, print_routing_table/1, get_all_routes/1]).
-import(key_gen, [hash_name/1, common_hex_prefix/2, hex_length/1]).
-import(utils, [get_time/0]).
-import(network, [send_message/2]).
-import(file_handler, [store_file/3, get_file_size/1, delete_file/1, list_files/1, move_file/2]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).
-import(node_actions, [full_route/4, update_list/5, broadcast_leaf/4, broadcast_leaf/3,
    broadcast_routing/4, broadcast_routing/3, send_file_to_store/4, get_file_path/2,
    save_file_to_store/4, get_backup_folder_path/2, get_backup_path/3, get_folder_path/1]).
-export([backup/3, backup_res/5, backup_update/4, backup_find/7, backup_found/4, new_leaf_backup/4,
  remove_backup_folder/2, remove_backup_file/3, old_leaf_backup/4, backup_remove/3, update_leaf_backups/4]).

-include_lib("kernel/include/file.hrl").

-define(EXPIRATION, 5000).


%sends backup to all nodes
backup({SelfAddr, SelfName}, LeafSet, FileName) ->
    FilePath = get_file_path(SelfName,  FileName),
    case file:read_file(FilePath) of
        {ok, FileData} ->
            FileSize = erlang:byte_size(FileData),
            broadcast_leaf({SelfAddr, SelfName}, LeafSet, {backup, FileName, FileSize, FileData});
        _ ->
            ok
    end.


%deletes all backups of a file
backup_remove(SelfInfo, LeafSet, FileName) ->
    broadcast_leaf(SelfInfo, LeafSet, {delete_backup, FileName}).


%saves a file in the backup frolder of From
backup_res({_SelfAddr, SelfName}, {_FromAddr, FromName}, FileName, FileSize, FileData) ->
    FilePath = get_backup_path(SelfName, FromName, FileName),
    store_file(FilePath, FileSize, FileData).

%updates owner of file, if not known looks for it
backup_update({SelfAddr, SelfName}, {_FromAddr, FromName}, RoutingTable, LeafSet) ->
    BackupFolder = get_backup_folder_path(SelfName, FromName),
    Files = list_files(BackupFolder),
    lists:foreach(fun(FileName) ->
        Key = hash_name(FileName),
        case full_route(SelfName, RoutingTable, LeafSet, Key) of
            route_end -> 
                DestPath = get_file_path(SelfName, FileName),
                SourcePath = get_backup_path(SelfName, FromName, FileName),
                move_file(SourcePath, DestPath),
                backup({SelfAddr, SelfName}, LeafSet, DestPath);
            {Pid, _} -> 
                Pid ! {{SelfAddr, SelfName}, make_ref(), get_time(), {backup_find, FileName, FromName}}
        end
    end, Files).


%looks for owner of file
backup_find({SelfAddr, SelfName}, {FromAddr, FromName}, Msg_id, RoutingTable, LeafSet, FileName, BackupName) ->
    Key = hash_name(FileName),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end ->
            FromAddr ! {{SelfAddr, SelfName}, Msg_id, get_time(), {backup_found, BackupName}};
            {Pid, _} -> Pid ! {{FromAddr, FromName}, Msg_id, get_time(), {backup_find, FileName, BackupName}}
    end.


%moves the fil to the right backup folder
backup_found({_SelfAddr, SelfName}, {_FromAddr, FromName}, FileName, BackupName) ->
    DestPath = get_backup_path(SelfName, FromName, FileName),
    SourcePath = get_backup_path(SelfName, BackupName, FileName),
    move_file(SourcePath, DestPath).


%sends its folder to a node as backup
backup_folder_to({SelfAddr, SelfName}, {FromAddr, _FromName}) ->
    FolderPath = get_folder_path(SelfName),
    Files = list_files(FolderPath),
    lists:foreach(fun(FileName) ->
        FilePath = get_file_path(SelfName, FileName),
        case file:read_file(FilePath) of
            {ok, FileData} ->
                FileSize = byte_size(FileData),
                FromAddr ! {{SelfAddr, SelfName}, make_ref(), get_time(), {backup, FileName, FileSize, FileData}};
            {error, _Reason} ->
                error
        end
    end, Files).


%for the nodes checks if they entered or exited leafset
update_leaf_backups(SelfInfo, NewLeafSet, {OldL, OldR}, NewNodes) ->
    lists:foreach(fun(Node) ->
        new_leaf_backup(SelfInfo, Node, NewLeafSet, {OldL, OldR})
    end, NewNodes),

    OldList = OldL ++ OldR,
    OldNodes = [Node || {_Key, Node} <- OldList],
    lists:foreach(fun(Node) ->
        old_leaf_backup(SelfInfo, Node, NewLeafSet,{OldL, OldR})
    end, OldNodes).


%checks if node entered Leafset
new_leaf_backup({SelfAddr, SelfName}, {FromAddr, FromName}, {L, R}, {OldL, OldR}) ->
    LeafSetNodes = L ++ R,
    OldLeafSetNodes = OldL ++ OldR,
    Leaf = {hash_name(FromName), {FromAddr, FromName}},

    case {lists:member(Leaf, LeafSetNodes), lists:member(Leaf, OldLeafSetNodes)} of
        {true, false} ->
            backup_folder_to({SelfAddr, SelfName}, {FromAddr, FromName});
        _ ->
            ok
    end.


%checks if node is not anymore in leafset
old_leaf_backup({SelfAddr, SelfName}, {FromAddr, FromName}, {L, R}, {OldL, OldR}) ->
    LeafSetNodes = L ++ R,
    OldLeafSetNodes = OldL ++ OldR,

    Leaf = {hash_name(FromName), {FromAddr, FromName}},
    case {lists:member(Leaf, LeafSetNodes), lists:member(Leaf, OldLeafSetNodes)} of
        {false, true} ->
            FromAddr ! {{SelfAddr, SelfName}, make_ref(), get_time(), {delete_backup_folder}};
        _ ->
            ok
    end.


%deletes backup folder of From
remove_backup_folder({_SelfAddr, SelfName}, {_FromAddr, FromName}) ->
    FolderPath = get_backup_folder_path(SelfName, FromName),
    Files = list_files(FolderPath),
    lists:foreach(fun(File) ->
        delete_file(filename:join(FolderPath, File))
    end, Files).


%deletes the backup of a file from a backupPath
remove_backup_file({_SelfAddr, SelfName}, {_FromAddr, FromName}, FileName) ->
    FilePath = get_backup_path(SelfName, FromName, FileName),
    delete_file(FilePath).
