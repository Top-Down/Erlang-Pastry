-module(node).
-import(node_actions, [join_response/6, keepalive/3,
   backup/3, share_info/3, store_file/1, store_response/6,
   exit_response/4,update_keepalive/4, check_expired_nodes/5, 
   backup_response/2, find/6, delete/6, suicide/3, update_list/5,
   join_update/7]).
-import(routing, [init_routing_table/1]).
-import(key_gen, [hash_name/1]).
-import(utils, [get_time/0]).
-import(network, [create_net_node/2]).
-export([start_node/2, start_node/1, start_node/3]).

-define(CHECK_INTERVAL, 15000).
-define(KEEPALIVE_INTERVAL, 3000).
-define(EXPIRATION_INTERVAL, 2000).
-define(L2, 2).

start_node(Name) ->
  spawn(fun() -> bootstrap_node(Name) end).
start_node(Name, Starter) ->
  spawn(fun() -> bootstrap_node(Name, Starter) end).
start_node(Name, Starter, LeafSet) ->
  spawn(fun() -> bootstrap_node(Name, Starter, LeafSet) end).

bootstrap_node(Name) ->
    bootstrap_node(Name, undefined, {[], []}).
bootstrap_node(Name, Starter) ->
    bootstrap_node(Name, Starter, {[], []}).
bootstrap_node(Name, Starter, LeafSet) ->
    {SelfName, RoutingTable, KeepAliveList} = init_node(Name),
    erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
    erlang:send_after(?CHECK_INTERVAL, self(), check_nodes),
    {NewRoutingTable, NewLeafSet} = join_net(SelfName, Starter, RoutingTable, LeafSet, ?L2),
    node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfName).


init_node(Name) ->
  % Start the node with a name and cookie
  %Cookie = "pastry",
  %SelfName = create_net_node(Name, Cookie),

  SelfName = Name,
  % Initialize routing table and keepalive list
  RoutingTable = init_routing_table(hash_name(Name)),
  KeepAliveList = [],
  {SelfName, RoutingTable, KeepAliveList}.


join_net(_, undefined,RoutingTable, LeafSet, _) ->
  {RoutingTable, LeafSet};
join_net(SelfName, {StarterPid, StarterName}, RoutingTable, {L, R}, L2) ->
    Leaves = L++R,
    {NewRoutingTable, NewLeafSet} = update_list(SelfName, [{StarterPid, StarterName} | Leaves], RoutingTable,{L,R}, L2),
    StarterPid ! {{self(), SelfName}, make_ref(), get_time(), {join}},
    io:format("Join sent from ~p to ~p~n", [SelfName, StarterName]),
    {NewRoutingTable, NewLeafSet}.


% Main loop for the provider node
node_loop(RoutingTable, LeafSet, KeepAliveList, SelfName) ->
  receive
    {From, Msg_id, Timestamp, {find, File}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      find(SelfName, From, Msg_id, RoutingTable, LeafSet, File),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfName);

    {From, Msg_id, Timestamp, {delete, File}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      delete(SelfName, From, Msg_id, RoutingTable, LeafSet, File),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfName);
    
    {From, Msg_id, Timestamp, {store, File}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      store_response(SelfName, From, Msg_id, RoutingTable, LeafSet, File),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfName);

    {From, Msg_id, Timestamp, {backup, File}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      backup_response(From, File),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfName);

    {From, Msg_id, Timestamp, {join}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_response(SelfName, From, Msg_id, RoutingTable, LeafSet, ?L2),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfName);

    {{FromPid, FromName}, Msg_id, Timestamp, {join_res, Row, SharedLeafSet}} ->
      NewKeepAliveList = update_keepalive({FromPid, FromName}, Msg_id, Timestamp, KeepAliveList),
      {NewRoutingTable, NewLeafSet} = join_update(SelfName, {FromPid, FromName}, Row, SharedLeafSet, RoutingTable, LeafSet, ?L2),
      node_loop(NewRoutingTable, NewLeafSet, NewKeepAliveList, SelfName);

    {From, _Msg_id, _Timestamp, {exit}} ->
      {NewRoutingTable, NewLeafSet} = exit_response(SelfName, From, RoutingTable, LeafSet),
      node_loop(NewRoutingTable, NewLeafSet, KeepAliveList, SelfName);

    {From, Msg_id, Timestamp, {alive}} ->
      NewKeepAliveList = update_keepalive(From, Msg_id, Timestamp, KeepAliveList),
      node_loop(RoutingTable, LeafSet, NewKeepAliveList, SelfName);

    send_keepalive ->
      keepalive(SelfName, RoutingTable, LeafSet),
      erlang:send_after(?KEEPALIVE_INTERVAL, self(), send_keepalive),
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfName);

    check_nodes ->
      {NewKeepAliveList, NewRoutingTable, NewLeafset} = check_expired_nodes(SelfName, KeepAliveList, RoutingTable, LeafSet, ?EXPIRATION_INTERVAL),
      erlang:send_after(?CHECK_INTERVAL, self(), check_nodes),
      node_loop(NewRoutingTable, NewLeafset, NewKeepAliveList, SelfName);

    kill_node ->
      suicide(SelfName, RoutingTable, LeafSet);
    
    _ ->
      node_loop(RoutingTable, LeafSet, KeepAliveList, SelfName)
  end.


