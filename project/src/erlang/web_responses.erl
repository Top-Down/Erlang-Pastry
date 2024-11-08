-module(node_actions).
-import(key_gen, [hash_name/1]).
-import(utils, [get_time/0]).
-import(file_handler, [list_files/1]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).
-import(node_actions, [full_route/4, broadcast/3, broadcast/4, broadcast_tree/4,
    send_file_to_store/5, save_file_to_store/4, delete_stored_file/5, get_folder_path/1]).

-export([find_store/6, store/7, find/6, delete/7, get_files_res/6, get_files_res_handle/2, 
    get_all_files/7, check_expired_blacklist/2, all_files_res/4]).


find_store({SelfAddr, SelfName}, {FromAddr, FromName}, Msg_id, RoutingTable, LeafSet, FileName) ->
    Key = hash_name(FileName),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> FromAddr ! {{SelfAddr, SelfName}, Msg_id, get_time(), {store_found, SelfAddr}};
        {Pid, _} -> Pid ! {{FromAddr, FromName}, Msg_id, get_time(), {find_store, FileName}}
    end.


store({SelfAddr, SelfName}, {FromAddr, _FromName}, Msg_id, FileName, FileData, FileSize, FilesList) ->
    Result = save_file_to_store({SelfAddr, SelfName}, FileName, FileData, FileSize),
    case Result of
        ok ->
            FromAddr ! {{SelfAddr, SelfName}, Msg_id, get_time(), {store_end}},
            FilesList ++ FileName;
        {error, Reason} ->
            FromAddr ! {{SelfAddr, SelfName}, Msg_id, get_time(), {error, Reason}},
            FilesList
    end.


find({SelfAddr, SelfName}, From, Msg_id, RoutingTable, LeafSet, FileName) ->
    Key = hash_name(FileName),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> send_file_to_store({SelfAddr, SelfName}, From, find_end, Msg_id, FileName);
        {Pid, _Name} -> Pid ! {From, Msg_id, get_time(), {find, FileName}}
    end.


delete({SelfAddr, SelfName}, From, Msg_id, RoutingTable, LeafSet, FileName, FilesList) ->
    Key = hash_name(FileName),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> 
            delete_stored_file({SelfAddr, SelfName}, From, delete_end, Msg_id, FileName),
            [File || File <- FilesList, File =/= FileName];
        Id -> 
            Id ! {From, Msg_id, get_time(), {delete, FileName}},
            FilesList
    end.


get_all_files({SelfAddr, SelfName}, From, Msg_Id, RoutingTable, LeafSet, BlackList, FloodTimeout) ->
    FilePath = get_folder_path(SelfName),
    FilesList = list_files(FilePath),
    NewMsg = {{SelfAddr, SelfName}, Msg_Id, get_time(), {files_req}},
    broadcast({SelfAddr, SelfName}, LeafSet, Msg_Id, NewMsg),
    broadcast_tree({SelfAddr, SelfName}, RoutingTable, Msg_Id, NewMsg),
    erlang:send_after(FloodTimeout, self(), {flood_end, From, Msg_Id}),
    {FilesList, [{Msg_Id, get_time()} | BlackList]}.


all_files_res(SelfInfo, {FromAddr, _FromName}, Msg_Id, FilesList)->
    FromAddr ! {SelfInfo, Msg_Id, get_time(), {all_files_res, FilesList}}.


get_files_res({SelfAddr, SelfName}, {FromPid, FromName}, Msg_Id, RoutingTable, LeafSet, BlackList)->
    case lists:keymember(1, Msg_Id, BlackList) of
        true -> BlackList;
        false ->        
            FilePath = get_folder_path(SelfName),
            AllFiles = list_files(FilePath),
            FromPid ! {{SelfAddr, SelfName}, Msg_Id, get_time(), {files_res, AllFiles}},    
            NewMsg = {files_req},
            broadcast({FromPid, FromName}, LeafSet, Msg_Id, NewMsg),
            broadcast_tree({FromPid, FromName}, RoutingTable, Msg_Id, NewMsg),
            [{Msg_Id, get_time()} | BlackList]
    end.


get_files_res_handle(OldList, NewFiles)->
    NewList = OldList ++ [File || File <- NewFiles, not lists:member(File, OldList)],
    NewList.


% Function to check for expired nodes and delete them from RoutingTable and Leafset
check_expired_blacklist(BlackList, ExpirationTime) ->
    CurrentTime = get_time(),
    FilteredList = lists:filter(fun({_, Timestamp}) -> Timestamp + ExpirationTime > CurrentTime end, BlackList),
    FilteredList.
