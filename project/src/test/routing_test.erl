-module(routing_test).

-include_lib("eunit/include/eunit.hrl").

% Import the modules to be tested
-import(key_gen, [hash_name/1]).
-import(routing, [init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/2, update_routing/2, get_all_routes/1]).

% Test data setup
setup() ->
    Node1 = {self(), "node1"},
    Node2 = {self(), "node2"},
    Node3 = {self(), "node3"},
    Node4 = {self(), "node4"},
    io:format("node1: ~p~n", [hash_name("node1")]),
    io:format("node2: ~p~n", [hash_name("node2")]),
    io:format("node3: ~p~n", [hash_name("node3")]),
    io:format("node4: ~p~n", [hash_name("node4")]),
    <<Key1/bitstring>> = hash_name("node1"),
    Table = init_routing_table(<<Key1/bitstring>>),
    {Node1, Node2, Node3, Node4, Table}.

% Test init_routing_table/1
init_routing_table_test() ->
    {_, _, _, _, {_TableKey, Table}} = setup(),
    ?assertEqual(32, length(Table)).


% Test add_node/2
add_node_test() ->
    {Node1, Node2, Node3, Node4, Table} = setup(),
    Table2 = add_node(Node2, Table),
    io:format("Table5: ~p~n", [Table2]),
    Table3 = add_node(Node1, Table2),
    io:format("Table5: ~p~n", [Table3]),
    Table4 = add_node(Node3, Table3),
    io:format("Table5: ~p~n", [Table4]),
    {_Key, Table5} = add_node(Node4, Table4),
    io:format("Table5: ~p~n", [Table5]),
    ?assertEqual(2, length(lists:nth(1, Table5))),
    ?assertEqual(1, length(lists:nth(2, Table5))).

% Test remove_node/2
remove_node_test() ->
    {Node1, Node2, Node3, Node4, Table} = setup(),
    Table2 = add_node(Node2, Table),
    Table3 = add_node(Node3, Table2),
    Table4 = add_node(Node4, Table3),
    Table5 = remove_node(Node3, Table4),
    Table6 = remove_node(Node1, Table5),
    {_Key, Table7} = remove_node(Node1, Table6),
    io:format("Table5: ~p~n", [Table7]),
    ?assertEqual(2, length(lists:nth(1, Table7))),
    ?assertEqual(0, length(lists:nth(2, Table7))).

% Test get_row/3
get_row_test() ->
    {_, Node2, Node3, Node4, Table} = setup(),
    Table2 = add_node(Node2, Table),
    Table3 = add_node(Node3, Table2),
    Table4 = add_node(Node4, Table3),
  
    Row = get_row(Table4, 0),
    io:format("Row: ~p~n", [Row]),
    ?assertEqual(2, length(Row)),
    Row2 = get_row(Table4, 1),
    io:format("Row: ~p~n", [Row]),
    ?assertEqual(1, length(Row2)).

% Test get_row/3
get_all_routes_test() ->
    {_, Node2, Node3, Node4, Table} = setup(),
    Table2 = add_node(Node2, Table),
    Table3 = add_node(Node3, Table2),
    Table4 = add_node(Node4, Table3),
    
    AllRoutes = get_all_routes(Table4),
    io:format("AllRoutes: ~p~n", [AllRoutes]),
    ?assertEqual(3, length(AllRoutes)).


% Test route_key/2
route_key_test() ->
    {_, Node2, Node3, Node4, Table} = setup(),
    Table2 = add_node(Node2, Table),
    Table3 = add_node(Node3, Table2),
    Table4 = add_node(Node4, Table3),
    {_, Next1} = route_key(hash_name("node1"), Table4),
    ?assertEqual(self, Next1),
    {_, Next2} = route_key(hash_name("node2"), Table4),
    ?assertEqual("node2", Next2),
    {_, Next3} = route_key(hash_name("node3"), Table4),
    ?assertEqual("node3", Next3),
    {_, Next4} = route_key(hash_name("node4"), Table4),
    ?assertEqual("node4", Next4).