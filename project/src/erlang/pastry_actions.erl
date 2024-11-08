-module(pastry_actions).
-import(routing, [init_routing_table/1, route_key/2, add_node/2, remove_node/2, get_row/3, update_routing/2, print_routing_table/1]).
-import(key_gen, [hash_name/1, common_hex_prefix/2, hex_length/1]).
-import(utils, [get_time/0]).
-import(network, [send_message/2]).
-import(file_handler, [send_file/2, receive_file/2, get_file_size/1, delete_file/1, list_files/1]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).
-import(node_actions, [full_route/4, update_list/5, broadcast/3, broadcast_tree/3, send_file_to_store/4]).
-export([join_res/6, keepalive/3, backup/3, share_info/3, exit_response/4, update_keepalive/4, check_expired_nodes/5, 
  backup_response/4, suicide/3, join_res_handle/7]).

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


join_res_handle({_SelfAddr, SelfName}, From, Row, {L,R}, RoutingTable, LeafSet, L2) ->
    NodesList = [V || {_, V} <- Row],
    LeavesList = L ++ R,
    AllLeaves = [S || {_, S} <- LeavesList],
    AllNodes = NodesList ++ AllLeaves,
    update_list(SelfName, [From | AllNodes], RoutingTable, LeafSet, L2).


backup(SelfInfo, DestList, FileName) ->
    broadcast(SelfInfo, DestList, {backup, FileName}).

backup_response(SelfName, From, Msg_id, FileName) ->
    send_file_to_store(SelfName, Msg_id, FileName, From).

keepalive(SelfInfo, RoutingTable, LeafSet) ->
    broadcast_tree(SelfInfo, RoutingTable, {alive}),
    broadcast(SelfInfo, LeafSet, {alive}).

share_info(SelfInfo, DestList, Info) ->
    broadcast(SelfInfo, DestList, {info, Info}).


exit_response({_SelfAddr, SelfName}, From, RoutingTable, LeafSet) ->
    NewLeafSet = remove_leaf(LeafSet,From, SelfName),
    NewRoutingTable = remove_node(From, RoutingTable),
    {NewRoutingTable, NewLeafSet}.


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
    {NewRoutingTable, NewLeafset} = lists:foldl(fun(Node, {RT, LS}) ->
        {remove_node(Node, RT), remove_leaf(LS, Node, SelfName)}
    end, {RoutingTable, LeafSet}, ExpiredNodes),
    NewAliveNodes = lists:filter(fun({_, Timestamp}) -> 
        CurrentTime - Timestamp < ExpirationTime end, AliveNodes),
    {NewAliveNodes, NewRoutingTable, NewLeafset}.


suicide(SelfInfo, RoutingTable, LeafSet) ->
    Msg = {exit},
    broadcast(SelfInfo, LeafSet, Msg),
    broadcast_tree(SelfInfo, RoutingTable, Msg),
    init:stop().
