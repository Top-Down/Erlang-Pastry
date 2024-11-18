-module(leaf_set_test).

-include_lib("eunit/include/eunit.hrl").

% Import the modules to be tested
-import(key_gen, [hash_name/1]).
-import(leaf_set, [add_leaf/4, remove_leaf/3, closest_node/3, update_leaf_set/4]).

% Test data setup
setup() ->
    SelfName = "self_node1",
    Node1 = {self(), "node1"},
    Node2 = {self(), "node2"},
    Node3 = {self(), "node3"},
    Node1Key = hash_name("node1"),
    Node2Key = hash_name("node2"),
    Node3Key = hash_name("node3"),
    io:format("Node1Key: ~p~n", [Node1Key]),
    io:format("Node2Key: ~p~n", [Node2Key]),
    io:format("Node3Key: ~p~n", [Node3Key]),
    io:format("SelfKey: ~p~n", [hash_name(SelfName)]),

    {SelfName, Node1, Node2, Node3}.

% Test add_leaf/4
add_leaf_test() ->
    {SelfName, Node1, Node2, _} = setup(),
    LeafSet = {[], []},
    L2 = 2,
    SelfKey = hash_name(SelfName),
    Node1Key = hash_name("node1"),
    Node2Key = hash_name("node2"),
    
    {NewLeft, NewRight} = add_leaf(LeafSet, L2, Node1, SelfName),
    {NewLeft1, NewRight1} = add_leaf({NewLeft, NewRight}, L2, {self(), SelfName}, SelfName),

    {NewLeft2, NewRight2} = add_leaf({NewLeft1, NewRight1}, L2, Node2, SelfName),
    
    % Check if Node1 should be on the left or right
    ExpectedLeft1 = if Node1Key < SelfKey -> 1; true -> 0 end,
    ExpectedRight1 = if Node1Key >= SelfKey -> 1; true -> 0 end,
    
    % Check if Node2 should be on the left or right
    ExpectedLeft2 = if Node2Key < SelfKey -> ExpectedLeft1 + 1; true -> ExpectedLeft1 end,
    ExpectedRight2 = if Node2Key >= SelfKey -> ExpectedRight1 + 1; true -> ExpectedRight1 end,
    
    ?assertEqual(ExpectedLeft1, length(NewLeft)),
    ?assertEqual(ExpectedRight1, length(NewRight)),
    ?assertEqual(ExpectedLeft2, length(NewLeft2)),
    ?assertEqual(ExpectedRight2, length(NewRight2)).

% Test remove_leaf/3
remove_leaf_test() ->
    {SelfName, Node1, Node2, _} = setup(),
    LeafSet = {[], []},
    L2 = 2,
    {NewLeft, NewRight} = add_leaf(LeafSet, L2, Node1, SelfName),
    {NewLeft2, NewRight2} = add_leaf({NewLeft, NewRight}, L2, Node2, SelfName),
    io:format("Before removal - NewLeft2: ~p, NewRight2: ~p~n", [NewLeft2, NewRight2]),
    {FinalLeft, FinalRight} = remove_leaf({NewLeft2, NewRight2}, Node1, SelfName),
    io:format("After removal - FinalLeft: ~p, FinalRight: ~p~n", [FinalLeft, FinalRight]),
    
    % Check if Node1 was correctly removed
    SelfKey = hash_name(SelfName),
    Node1Key = hash_name("node1"),
    ExpectedLeft = if Node1Key < SelfKey -> 0; true -> length(NewLeft2) end,
    ExpectedRight = if Node1Key >= SelfKey -> 0; true -> length(NewRight2) end,

    ?assertEqual(ExpectedLeft, length(FinalLeft)),
    ?assertEqual(ExpectedRight, length(FinalRight)).

% Test closest_node/3
closest_node_test() ->
    {SelfName, Node1, Node2, Node3} = setup(),
    LeafSet = {[], []},
    L2 = 2,
    {NewLeft, NewRight} = add_leaf(LeafSet, L2, Node1, SelfName),
    {NewLeft2, NewRight2} = add_leaf({NewLeft, NewRight}, L2, Node2, SelfName),
    {NewLeft3, NewRight3} = add_leaf({NewLeft2, NewRight2}, L2, Node3, SelfName),
    Key = hash_name(<<"node1">>),
    {_ , {_ , NodeId}} = closest_node({NewLeft3, NewRight3}, Key, SelfName),
    ?assertMatch("node1", NodeId).


% Test update_leaf_set/4
update_leaf_set_test() ->
    {SelfName, Node1, Node2, Node3} = setup(),
    LeafSet = {[], []},
    L2 = 2,
    Nodes = [Node1, Node2, Node3],
    UpdatedLeafSet = update_leaf_set(Nodes, LeafSet, L2, SelfName),
    {Left, Right} = UpdatedLeafSet,
    ?assertEqual(3, length(Left) + length(Right)).
