-module(routing_test).

-include_lib("eunit/include/eunit.hrl").

% Import the modules to be tested
-import(key_gen, [hash_name/1]).
-import(routing, [init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/3]).

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
    {_, _, _, _, Table} = setup(),
    print_routing_table(Table),
    ?assertEqual(1, length(Table)).


% Test add_node/2
add_node_test() ->
    {_, Node2, Node3, Node4, Table} = setup(),
    Table2 = add_node(Node2, Table),
    Table3 = add_node(Node3, Table2),
    print_routing_table(Table3),
    Table4 = add_node(Node4, Table3),
    ?assertEqual(3, length(Table4)).

% Test remove_node/2
remove_node_test() ->
    {_, Node2, Node3, Node4, Table} = setup(),
    Table2 = add_node(Node2, Table),
    Table3 = add_node(Node3, Table2),
    Table4 = add_node(Node4, Table3),
    Table5 = remove_node(Node3, Table4),
    ?assertEqual(3, length(Table5)).

% Test get_row/3
get_row_test() ->
    {_, Node2, Node3, Node4, Table} = setup(),
    Key1 = hash_name("node1"),
    Table2 = add_node(Node2, Table),
    Table3 = add_node(Node3, Table2),
    Table4 = add_node(Node4, Table3),
    
    Row = get_row(<<Key1/bitstring>>, Table4, 0),
    ?assertEqual(3, length(Row)).


% Test route_key/2
route_key_test() ->
    {_, Node2, Node3, Node4, Table} = setup(),
    Table2 = add_node(Node2, Table),
    Table3 = add_node(Node3, Table2),
    Table4 = add_node(Node4, Table3),
    {{_, Next1}, _} = route_key(hash_name("node1"), Table4),
    ?assertEqual(self, Next1),
    {{_, Next2}, _} = route_key(hash_name("node2"), Table4),
    ?assertEqual("node2", Next2),
    {{_, Next3}, _} = route_key(hash_name("node3"), Table4),
    ?assertEqual("node3", Next3),
    {{_, Next4}, _} = route_key(hash_name("node4"), Table4),
    ?assertEqual("node4", Next4).