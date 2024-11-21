-module(node_actions).
-import(routing, [init_routing_table/1, route_key/2, add_node/2, remove_node/2, get_row/3, update_routing/2, print_routing_table/1, get_all_routes/1]).
-import(key_gen, [hash_name/1, common_hex_prefix/2, hex_length/1]).
-import(utils, [get_time/0]).
-import(network, [send_message/2]).
-import(file_handler, [store_file/3, get_file_size/1, delete_file/1, list_files/1]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).

-export([full_route/4, update_list/5, send_file_to_store/5, save_file_to_store/4, delete_stored_file/5,
    broadcast/4, broadcast_routing/4, broadcast_routing/3, get_folder_path/1, get_file_path/2,
    get_backup_folder_path/2, get_backup_path/3, broadcast_leaf/4, broadcast_leaf/3]).

-include_lib("kernel/include/file.hrl").

-define(EXPIRATION, 5000).

%routes key using routing table and Leafset
full_route(SelfName, RoutingTable, LeafSet, Key) ->
    case route_key(Key, RoutingTable) of
        {self, self} ->
            case closest_node(LeafSet, Key, SelfName) of
                self -> route_end;
                {_, NodeInfo} -> NodeInfo
            end;
        {no_match, no_match} ->
            case closest_node(LeafSet, Key, SelfName) of
                self -> route_end;
                {_, NodeInfo} -> NodeInfo
            end; 
        NodeInfo -> NodeInfo
    end.


%given a list, updates the routing table and leaf set
update_list(SelfName, NodesList, RoutingTable, LeafSet, L2) ->
    NewRoutingTable = update_routing(RoutingTable, NodesList),
    NewLeafSet = update_leaf_set(NodesList, LeafSet, L2, SelfName),
    {NewRoutingTable, NewLeafSet}.


get_folder_path(SelfName) ->
    FolderPath = "./files/" ++ SelfName ++ "/",
    file:make_dir(FolderPath),
    FolderPath.

get_file_path(SelfName, FileName) ->
    "./files/" ++ SelfName ++ "/"++ FileName.


%gets backup folder for a node
get_backup_folder_path(SelfName, NodeName) ->
    FolderPath = "./files/" ++ SelfName ++ "/backup/"++ NodeName ++ "/",
    file:make_dir(FolderPath),
    FolderPath.

%finds backup path for a file
get_backup_path(SelfName, NodeName, FileName) ->
    "./files/" ++ SelfName ++ "/backup/"++ NodeName ++ "/" ++ FileName.


%saves file
save_file_to_store({_SelfAddr, SelfName}, FileName, FileSize, FileData) ->
    FilePath = get_file_path(SelfName, FileName),
    store_file(FilePath, FileSize, FileData).


%sends a file
send_file_to_store({SelfAddr, SelfName}, {FromPid, _FromName}, OpCode, Msg_id, FileName) ->
    FilePath = get_file_path(SelfName, FileName),
    case file:read_file(FilePath) of
        {ok, Data} ->            
            Size = erlang:byte_size(Data),
            FromPid ! {{SelfAddr, SelfName}, Msg_id, get_time(), {OpCode, FileName, Size, Data}};
        {error, Reason} ->
            FromPid ! {{SelfAddr, SelfName}, Msg_id, get_time(), {error, Reason}}
    end.


%deletes file and sends message back
delete_stored_file({SelfAddr, SelfName}, {FromAddr, _FromName}, OpCode, Msg_Id, FileName) ->
    FilePath = get_file_path(SelfName, FileName),
    delete_file(FilePath),
    FromAddr ! {{SelfAddr, SelfName}, Msg_Id, get_time(), {OpCode}}.


%broadcast to leafset
broadcast_leaf(SelfInfo, NodeList, Msg) ->
    broadcast_leaf(SelfInfo, NodeList, make_ref(), Msg).
broadcast_leaf(SelfInfo, {L,R}, Msg_Id, Msg) ->
    Leaves = L++R,
    NodeList = [Node || {_Key, Node} <- Leaves],
    broadcast(SelfInfo, NodeList, Msg_Id, Msg).


%broadcast to full routing table
broadcast_routing(SelfInfo, RoutingTable, Msg) ->
    broadcast_routing(SelfInfo, RoutingTable, make_ref(), Msg).

broadcast_routing(SelfInfo, RoutingTable, Msg_Id, Msg) ->
    NodeList = get_all_routes(RoutingTable),
    broadcast(SelfInfo, NodeList, Msg_Id, Msg).


%broadcast to list
broadcast(_, [], _,_) ->
    done;
broadcast(SelfInfo, [{NodePid, _Name}|T], Msg_id, Msg) ->
    NodePid ! {SelfInfo, Msg_id, get_time(), Msg},
    broadcast(SelfInfo, T, Msg_id, Msg).

