-module(node_actions).
-import(routing, [init_routing_table/1, route_key/2, add_node/2, remove_node/2, get_row/3, update_routing/2, print_routing_table/1]).
-import(key_gen, [hash_name/1, common_hex_prefix/2, hex_length/1]).
-import(utils, [get_time/0]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).
-export([join_response/6, keepalive/3, backup/3, share_info/3, store_file/1, 
  store_response/6, exit_response/4, update_keepalive/4, check_expired_nodes/5, 
  backup_response/2, find/6, delete/6, suicide/3, full_route/4, join_update/7, update_list/5]).

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


join_response(SelfName, {FromPid, FromName}, Msg_id, RoutingTable, LeafSet, L2) ->
    Key = hash_name(FromName),
    {_, UsedKeys} = route_key(Key, RoutingTable),
    I = hex_length(UsedKeys),
    Row = get_row(hash_name(SelfName), RoutingTable, I),
    FromPid ! {{self(), SelfName}, Msg_id, get_time(), {join_res, Row, LeafSet}},

    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> route_end;
        {Pid, _} -> Pid ! {{FromPid, FromName}, Msg_id, get_time(), {join}}
    end,

    RoutingTable1 = add_node({FromPid, FromName}, RoutingTable),
    LeafSet1 = add_leaf(LeafSet, L2, {FromPid, FromName}, SelfName),
    {RoutingTable1, LeafSet1}.


join_update(SelfName, From, Row, {L,R}, RoutingTable, LeafSet, L2) ->

    io:format("Join by ~p with leaves: ~p~n", [SelfName, LeafSet]),
    io:format("Join by ~p with row: ~p~n", [SelfName, Row]),
    NodesList = [V || {_, V} <- Row],
    LeavesList = L ++ R,
    AllLeaves = [S || {_, S} <- LeavesList],
    AllNodes = NodesList ++ AllLeaves,
    update_list(SelfName, [From | AllNodes], RoutingTable, LeafSet, L2).


update_list(SelfName, NodesList, RoutingTable, LeafSet, L2) ->
    NewRoutingTable = update_routing(RoutingTable, NodesList),
    NewLeafSet = update_leaf_set(NodesList, LeafSet, L2, SelfName),
    {NewRoutingTable, NewLeafSet}.


store_response(SelfName, From, Msg_id, RoutingTable, LeafSet, File) ->
    Key = hash_name(File),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> store_file(File);
        {Pid, _} -> Pid ! {From, Msg_id, get_time(), {store, File}}
    end.


find(SelfName, From, Msg_id, RoutingTable, LeafSet, File) ->
    Key = hash_name(File),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> 
            io:fwrite("Route end ~p~n", [SelfName]),
            print_routing_table(RoutingTable),
            send_file(SelfName, File, From);
        {Pid, Name} -> 
            io:fwrite("Next hop ~p: ~p ~n", [Pid, Name]),
            Pid ! {From, Msg_id, get_time(), {find, File}}
    end.


delete(SelfName, From, Msg_id, RoutingTable, LeafSet, File) ->
    Key = hash_name(File),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> delete_file(File);
        Id -> Id ! {From, Msg_id, get_time(), {delete, File}}
    end.


store_file(File) ->
    io:fwrite("File stored: ~s~n", [File]).


send_file(SelfName, File, {FromPid, FromName}) ->
    io:fwrite("File File~s: from ~p to ~p~n", [File, SelfName, FromName]),
    FromPid ! {file_reply, get_time(), {SelfName, File}}.
    

delete_file(File) ->
    io:fwrite("File deleted: ~s~n", [File]).


broadcast(SelfName, {L,R}, Msg) ->
    broadcast(SelfName, L++R, Msg);
broadcast(_, [], _) ->
    done;
broadcast(SelfName, [{_, {NodePid, _Name}}|T], Msg) ->
    NodePid ! {{self(), SelfName}, make_ref(), get_time(), Msg},
    broadcast(SelfName, T, Msg).

broadcast_tree(_, [], _) ->
    done;
broadcast_tree(SelfName, [{_, H}|T], Msg) when is_list(H) ->
    broadcast_tree(SelfName, H, Msg),
    broadcast_tree(SelfName, T, Msg);
broadcast_tree(SelfName, [{_, {self, _}}|T], Msg) ->
    broadcast_tree(SelfName, T, Msg);
broadcast_tree(SelfName, [{_, {NodePid, _Name}}|T], Msg) ->
    NodePid ! {{self(), SelfName}, make_ref(), get_time(), Msg},
    broadcast_tree(SelfName, T, Msg).

backup(SelfName, DestList, File) ->
    broadcast(SelfName, DestList, {backup, File}).

backup_response(From, File) ->
    io:fwrite("File stored: ~s from ~s~n", [File, From]).

keepalive(SelfName, RoutingTable, LeafSet) ->
    broadcast_tree(SelfName, RoutingTable, {alive}),
    broadcast(SelfName, LeafSet, {alive}).

share_info(SelfName, DestList, Info) ->
    broadcast(SelfName, DestList, {info, Info}).

exit_response(SelfName, From, RoutingTable, LeafSet) ->
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
check_expired_nodes(SelfName, AliveNodes, RoutingTable, LeafSet, ExpirationTime) ->
    CurrentTime = get_time(),
    ExpiredNodes = lists:filter(fun({_, Timestamp}) -> 
        CurrentTime - Timestamp >= ?EXPIRATION end, AliveNodes),
    {NewRoutingTable, NewLeafset} = lists:foldl(fun(Node, {RT, LS}) ->
        {remove_node(Node, RT), remove_leaf(LS, Node, SelfName)}
    end, {RoutingTable, LeafSet}, ExpiredNodes),
    NewAliveNodes = lists:filter(fun({_, Timestamp}) -> 
        CurrentTime - Timestamp < ExpirationTime end, AliveNodes),
    {NewAliveNodes, NewRoutingTable, NewLeafset}.


suicide(SelfName, RoutingTable, LeafSet) ->
    Msg = {exit},
    broadcast(SelfName, LeafSet, Msg),
    broadcast_tree(SelfName, RoutingTable, Msg),
    init:stop().
