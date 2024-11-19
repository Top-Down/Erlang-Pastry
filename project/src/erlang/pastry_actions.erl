-module(pastry_actions).
-import(routing, [init_routing_table/1, route_key/2, add_node/2, remove_node/2, get_row/2, update_routing/2, print_routing_table/1, get_all_routes/1]).
-import(key_gen, [hash_name/1, common_hex_prefix/2, hex_length/1]).
-import(utils, [get_time/0]).
-import(network, [send_message/2]).
-import(file_handler, [store_file/3, get_file_size/1, delete_file/1, list_files/1, move_file/2]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).
-import(node_actions, [full_route/4, update_list/5, send_file_to_store/5, save_file_to_store/4, delete_stored_file/5,
    broadcast/4, broadcast_routing/4, broadcast_routing/3, get_folder_path/1, get_file_path/2,
    get_backup_folder_path/2, get_backup_path/3, broadcast_leaf/4, broadcast_leaf/3]).

-import(backup_actions, [backup/3, backup_res/5, backup_update/4, backup_find/7, backup_found/4, new_leaf_backup/4,
    remove_backup_folder/2, remove_backup_file/3, old_leaf_backup/4, backup_remove/3, update_leaf_backups/4]).

-export([join_res/6, keepalive/3, share_info/3, exit_response/5, update_keepalive/4, check_expired_nodes/5, 
    suicide/3, join_res_handle/7, info_res/5, send_alive_res/3, refresh_file_owner/3]).

-include_lib("kernel/include/file.hrl").

-define(EXPIRATION, 5000).


join_res({SelfAddr, SelfName}, {FromPid, FromName}, Msg_id, {TableKey, Table}, LeafSet, L2) ->
    Key = hash_name(FromName),

    RowId = common_hex_prefix(Key, TableKey),
    Row = get_row({TableKey, Table}, RowId),
    FromPid ! {{SelfAddr, SelfName}, Msg_id, get_time(), {join_res, Row, LeafSet}},

    case full_route(SelfName, {TableKey, Table}, LeafSet, Key) of
        route_end -> route_end;
        {Pid, _NextName} -> 
            Pid ! {{FromPid, FromName}, Msg_id, get_time(), {join}}
    end,

    RoutingTable1 = add_node({FromPid, FromName}, {TableKey, Table}),
    LeafSet1 = add_leaf(LeafSet, L2, {FromPid, FromName}, SelfName),
    {RoutingTable1, LeafSet1}.


join_res_handle({SelfAddr, SelfName}, From, Row, {L,R}, RoutingTable, LeafSet, L2) ->
    LeavesList = L ++ R,
    AllLeaves = [S || {_, S} <- LeavesList],
    AllNodes = Row ++ AllLeaves,
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, [From | AllNodes], RoutingTable, LeafSet, L2),

    update_leaf_backups({SelfAddr, SelfName}, NewLeafSet, LeafSet, [From | AllNodes]),
    {NewRoutingTable, NewLeafSet}.


refresh_file_owner({SelfAddr, SelfName}, RoutingTable, LeafSet) ->
    Folder = get_folder_path(SelfName),
    Files = list_files(Folder),

    lists:foreach(fun(FileName) ->
        Key = hash_name(FileName),
        case full_route(SelfName, RoutingTable, LeafSet, Key) of
            route_end -> mine;
            NewOwner -> 
                send_file_to_store({SelfAddr, SelfName}, NewOwner, store, make_ref(), FileName),
                FilePath = get_file_path(SelfName, FileName),
                delete_file(FilePath),
                backup_remove({SelfAddr, SelfName}, LeafSet, FileName)
        end
    end, Files).


keepalive(SelfInfo, RoutingTable, LeafSet) ->
    broadcast_routing(SelfInfo, RoutingTable, {alive}),
    broadcast_leaf(SelfInfo, LeafSet, {alive}).


send_alive_res(SelfInfo, {FromAddr, _FromName}, Msg_Id) ->
    FromAddr ! {SelfInfo, Msg_Id, get_time(), {alive_res}}.


share_info(SelfInfo, DestList, {L, R}) ->
    Leaves = L ++ R,
    NodesList = [Node || {_Key, Node} <- Leaves],
    broadcast_leaf(SelfInfo, DestList, {info, NodesList}).


info_res({_SelfAddr, SelfName}, NodesList, RoutingTable, LeafSet, L2) ->
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, NodesList, RoutingTable, LeafSet, L2),
    {NewRoutingTable, NewLeafSet}.


exit_response({SelfAddr, SelfName}, From, RoutingTable, LeafSet, L2) ->
    CleanLeafSet = remove_leaf(LeafSet,From, SelfName),
    NewRoutingTable = remove_node(From, RoutingTable),
    AllRoutes = get_all_routes(NewRoutingTable),
    {NewLeft, NewRight} = update_leaf_set(AllRoutes, CleanLeafSet, L2, SelfName),
    
    NewLeavesList = NewLeft ++ NewRight,
    NewLeaves = [Node || {_Key, Node} <- NewLeavesList],
    lists:foreach(fun(Node) ->
        new_leaf_backup({SelfAddr, SelfName}, Node, {NewLeft, NewRight}, CleanLeafSet) 
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
    broadcast_leaf(SelfInfo, LeafSet, Msg),
    broadcast_routing(SelfInfo, RoutingTable, Msg),
    io:format("~p See you space cowboy... ~n", [SelfInfo]),
    exit(normal).