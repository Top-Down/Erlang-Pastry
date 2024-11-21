-module(leaf_set).

-import(key_gen, [hash_name/1, find_longest_shared_prefix/2, shared_prefix/2, compare_prefix/4, find_closest_id/2, find_closest/4]).
%-import(network, [create_connection/1]).

-export([add_leaf/4, remove_leaf/3, closest_node/3, update_leaf_set/4]).


%adds node to a list
add_to_list(List, MaxLen, Element, RemoveFun) ->
    case lists:member(Element, List) of
        true ->
            List;
        false ->
            case length(List) < MaxLen of
                true ->
                    [Element | List];
                false ->
                    List1 = [Element | List],
                    ElementToRemove = RemoveFun(List1),
                    lists:delete(ElementToRemove, List1)
            end
    end.

%adds leaf in the right list
add_leaf(LeafSet, _, {_NodePid, NodeName}, SelfName) 
  when NodeName =:= SelfName -> 
    LeafSet;
add_leaf(LeafSet, _, {_NodePid, NodeName}, _SelfName) 
  when not is_list(NodeName) -> 
    LeafSet;
add_leaf({Left, Right}, L2, {NodePid, NodeName}, SelfName) ->
    NodeKey = hash_name(NodeName),
    MyKey = hash_name(SelfName),
    NodeTuple = {NodeKey, {NodePid, NodeName}},

    case NodeKey < MyKey of
        true ->
            NewLeft = add_to_list(Left, L2, NodeTuple, fun lists:min/1),
            {NewLeft, Right};
        false ->
            NewRight = add_to_list(Right, L2, NodeTuple, fun lists:max/1),
            {Left, NewRight}
    end.


%remove leaf if exists
remove_leaf(LeafSet, {_NodePid, NodeName}, _SelfName) 
  when not is_list(NodeName) -> 
    LeafSet;
remove_leaf({Left, Right}, {NodePid, NodeName}, SelfName) ->
    NodeKey = hash_name(NodeName),
    MyKey = hash_name(SelfName),
    NodeTuple = {NodeKey, {NodePid, NodeName}},
    case NodeKey < MyKey of
        true ->
            NewLeft = lists:delete(NodeTuple, Left),
            {NewLeft, Right};
        false ->
            NewRight = lists:delete(NodeTuple, Right),
            {Left, NewRight}
    end.


%given a lsits, adds all the leafs
update_leaf_set([], LeafSet, _, _) ->
    LeafSet;
update_leaf_set([H|T], LeafSet, L2, SelfName) ->
    NewSet = add_leaf(LeafSet, L2, H, SelfName),
    update_leaf_set(T, NewSet, L2, SelfName).


%finds closest ndoe to a key
%if self is the closes, returns self
closest_node({Left, Right}, Key, SelfName) ->
    AllNodes = Left ++ Right,
    SelfKey = hash_name(SelfName),
    NodesWithSelf = [{SelfKey, {self(), SelfName}} | AllNodes],
    {BestId, _} = lists:foldl(fun(Id, {BestId, BestDiff}) -> 
        find_closest(Key, Id, BestId, BestDiff) 
    end, {<<>>, 0}, [NodeKey || {NodeKey, _} <- NodesWithSelf]),
    case lists:keyfind(BestId, 1, NodesWithSelf) of
        {SelfKey, _} -> self;
        {BestId, NodeInfo} -> {BestId, NodeInfo};
        false -> {error, not_found}
    end.