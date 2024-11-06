-module(node_actions).
-import(routing, [init_routing_table/1, route_key/2, add_node/2, remove_node/2, get_row/3, update_routing/2, print_routing_table/1]).
-import(key_gen, [hash_name/1, common_hex_prefix/2, hex_length/1]).
-import(utils, [get_time/0]).
-import(network, [send_message/2]).
-import(file_handler, [send_file/2, receive_file/2, get_file_size/1, delete_file/1, list_files/1]).
-import(leaf_set, [closest_node/3, remove_leaf/3, add_leaf/4, update_leaf_set/4]).
-export([join_res/6, keepalive/3, backup/3, share_info/3, receive_file_to_store/3, 
  store_response/6, exit_response/4, update_keepalive/4, check_expired_nodes/5, 
  backup_response/4, find/6, delete/6, suicide/3, full_route/4, join_res_handle/7, update_list/5,
  send_file_to_store/4, get_files_res/6, get_files_res_handle/2, get_all_files/7, check_expired_blacklist/2,
  all_files_res/4]).

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


update_list(SelfName, NodesList, RoutingTable, LeafSet, L2) ->
    NewRoutingTable = update_routing(RoutingTable, NodesList),
    NewLeafSet = update_leaf_set(NodesList, LeafSet, L2, SelfName),
    {NewRoutingTable, NewLeafSet}.


store_response({SelfAddr, SelfName}, From, Msg_id, RoutingTable, LeafSet, FileName) ->
    Key = hash_name(FileName),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> From ! {{SelfAddr, SelfName}, Msg_id, get_time(), {store_end, FileName}};
        {Pid, _} -> Pid ! {From, Msg_id, get_time(), {store, FileName}}
    end.


find({SelfAddr, SelfName}, From, Msg_id, RoutingTable, LeafSet, FileName) ->
    Key = hash_name(FileName),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> send_file_to_store({SelfAddr, SelfName}, Msg_id, FileName, From);
        {Pid, _Name} -> Pid ! {From, Msg_id, get_time(), {find, FileName}}
    end.


delete({SelfAddr, SelfName}, From, Msg_id, RoutingTable, LeafSet, FileName) ->
    Key = hash_name(FileName),
    case full_route(SelfName, RoutingTable, LeafSet, Key) of
        route_end -> delete_stored_file({SelfAddr, SelfName}, From, Msg_id, FileName);
        Id -> Id ! {From, Msg_id, get_time(), {delete, FileName}}
    end.

get_all_files({SelfAddr, SelfName}, From, Msg_Id, RoutingTable, LeafSet, BlackList, FloodTimeout) ->
    NewMsg = {{SelfAddr, SelfName}, Msg_Id, get_time(), {files_req}},
    broadcast({SelfAddr, SelfName}, LeafSet, Msg_Id, NewMsg),
    broadcast_tree({SelfAddr, SelfName}, RoutingTable, Msg_Id, NewMsg),
    erlang:send_after(FloodTimeout, self(), {flood_end, From, Msg_Id}),
    [{Msg_Id, get_time()} | BlackList].


all_files_res(SelfInfo, {FromAddr, _FromName}, Msg_Id, FilesList)->
    FromAddr ! {SelfInfo, Msg_Id, get_time(), {all_files_res, FilesList}}.


get_files_res({SelfAddr, SelfName}, {FromPid, FromName}, Msg_Id, RoutingTable, LeafSet, BlackList)->
    case lists:keymember(1, Msg_Id, BlackList) of
        true -> BlackList;
        false ->        
            FilePath = "./files/" ++ SelfName,
            AllFiles = list_files(FilePath),
            FromPid ! {{SelfAddr, SelfName}, Msg_Id, get_time(), {files_res, AllFiles}},    
            NewMsg = {files_req},
            broadcast({FromPid, FromName}, LeafSet, NewMsg),
            broadcast_tree({FromPid, FromName}, RoutingTable, Msg_Id, NewMsg),
            [{Msg_Id, get_time()} | BlackList]
    end.


get_files_res_handle(OldList, NewFiles)->
    NewList = OldList ++ [File || File <- NewFiles, not lists:member(File, OldList)],
    NewList.


receive_file_to_store({_SelfAddr, SelfName}, FileName, Size) ->
    FilePath = "./files/" ++ SelfName ++ "/"++ FileName,
    receive_file(FilePath, Size).


send_file_to_store({SelfAddr, SelfName}, Msg_id, FileName, {FromPid, _FromName}) ->
    FilePath = "./files/" ++ SelfName ++ "/"++ FileName,
    {_Res, Size} = get_file_size(FilePath),
    FromPid ! {{SelfAddr, SelfName}, Msg_id, get_time(), {file_send, FileName, Size}},
    send_file(FromPid, FilePath).
    

delete_stored_file({SelfAddr, SelfName}, {FromAddr, _FromName}, Msg_Id, FileName) ->
    FilePath = "./files/" ++ SelfName ++ "/"++ FileName,
    delete_file(FilePath),
    FromAddr ! {{SelfAddr, SelfName}, Msg_Id, get_time(), {store_end}}.


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


% Function to check for expired nodes and delete them from RoutingTable and Leafset
check_expired_blacklist(BlackList, ExpirationTime) ->
    CurrentTime = get_time(),
    FilteredList = lists:filter(fun({_, Timestamp}) -> Timestamp + ExpirationTime > CurrentTime end, BlackList),
    FilteredList.



suicide(SelfInfo, RoutingTable, LeafSet) ->
    Msg = {exit},
    broadcast(SelfInfo, LeafSet, Msg),
    broadcast_tree(SelfInfo, RoutingTable, Msg),
    init:stop().
