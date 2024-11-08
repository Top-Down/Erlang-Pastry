-module(node_actions).
-import(routing, [init_routing_table/1, route_key/2, add_node/2, remove_node/2, get_row/3, update_routing/2, print_routing_table/1]).
-import(key_gen, [hash_name/1, common_hex_prefix/2, hex_length/1]).
-import(utils, [get_time/0]).
-import(network, [send_message/2]).
-import(file_handler, [send_file/2, store_file/3, get_file_size/1, delete_file/1, list_files/1]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).
-export([full_route/4, update_list/5, send_file_to_store/5, save_file_to_store/4, delete_stored_file/5,
    broadcast/3, broadcast/4, broadcast_tree/4, broadcast_tree/3, get_folder_path/1, get_file_path/2]).

-include_lib("kernel/include/file.hrl").

-define(EXPIRATION, 5000).

full_route(SelfName, RoutingTable, LeafSet, Key) ->
    case route_key(Key, RoutingTable) of
        {{self, _}, _} ->
            case closest_node(LeafSet, Key, SelfName) of
                self -> route_end;
                {_, NodeInfo} -> NodeInfo
            end;
        {{no_match, _}, _} ->
            case closest_node(LeafSet, Key, SelfName) of
                self -> route_end;
                {_, NodeInfo} -> NodeInfo
            end; 
        {NodeInfo, _} -> NodeInfo
    end.


update_list(SelfName, NodesList, RoutingTable, LeafSet, L2) ->
    NewRoutingTable = update_routing(RoutingTable, NodesList),
    NewLeafSet = update_leaf_set(NodesList, LeafSet, L2, SelfName),
    {NewRoutingTable, NewLeafSet}.


get_folder_path(SelfName) ->
    "./files/" ++ SelfName.

get_file_path(SelfName, FileName) ->
    "./files/" ++ SelfName ++ "/"++ FileName.


save_file_to_store({_SelfAddr, SelfName}, FileName, FileData, FileSize) ->
    FilePath = get_file_path(SelfName, FileName),
    store_file(FilePath, FileData, FileSize).


send_file_to_store({SelfAddr, SelfName}, {FromPid, _FromName}, OpCode, Msg_id, FileName) ->
    FilePath = get_file_path(SelfName, FileName),
    {_Res, Size} = get_file_size(FilePath),
    case file:read_file(FilePath) of
        {ok, Data} ->
            FromPid ! {{SelfAddr, SelfName}, Msg_id, get_time(), {OpCode, Data, Size}};
        {error, Reason} ->
            FromPid ! {{SelfAddr, SelfName}, Msg_id, get_time(), {error, Reason}}
    end.


delete_stored_file({SelfAddr, SelfName}, {FromAddr, _FromName}, OpCode, Msg_Id, FileName) ->
    FilePath = get_file_path(SelfName, FileName),
    delete_file(FilePath),
    FromAddr ! {{SelfAddr, SelfName}, Msg_Id, get_time(), {OpCode}}.


broadcast(SelfInfo, {L,R}, Msg) ->
    broadcast(SelfInfo, L++R, make_ref(), Msg).

broadcast(SelfInfo, {L,R}, Msg_Id, Msg) ->
    broadcast(SelfInfo, L++R, Msg_Id, Msg);
broadcast(_, [], _,_) ->
    done;
broadcast(SelfInfo, [{_, {NodePid, _Name}}|T], Msg_id, Msg) ->
    NodePid ! {SelfInfo, Msg_id, get_time(), Msg},
    broadcast(SelfInfo, T, Msg_id, Msg).


broadcast_tree(SelfInfo, RoutingTable, Msg) ->
    broadcast_tree(SelfInfo, RoutingTable, make_ref(), Msg).

broadcast_tree(_, [], _, _) ->
    done;
broadcast_tree(SelfInfo, [{_, H}|T], Msg_Id, Msg) when is_list(H) ->
    broadcast_tree(SelfInfo, H, Msg_Id, Msg),
    broadcast_tree(SelfInfo, T, Msg_Id, Msg);
broadcast_tree(SelfInfo, [{_, {self, _}}|T], Msg_Id, Msg) ->
    broadcast_tree(SelfInfo, T, Msg_Id, Msg);
broadcast_tree(SelfInfo, [{_, {NodePid, _Name}}|T], Msg_Id, Msg) ->
    NodePid ! {SelfInfo, Msg_Id, get_time(), Msg},
    broadcast_tree(SelfInfo, T, Msg_Id, Msg).
