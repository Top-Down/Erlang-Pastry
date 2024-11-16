-module(routing).

-import(key_gen, [hash_name/1, binary_to_hex/1]).

-export([init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/3, update_routing/2, get_all_routes/1]).

split_binary_to_hex(Binary) ->
    split_binary_to_hex(Binary, []).

split_binary_to_hex(<<>>, Acc) ->
    Acc;
split_binary_to_hex(<<Hex:4, Rest/bitstring>>, Acc) ->
    split_binary_to_hex(Rest, [<<Hex:4>> | Acc]).


init_routing_table([], Acc) ->
    Acc;
init_routing_table([H | T], Acc) ->
    init_routing_table(T, [{H, Acc}]).

init_routing_table(<<NodeKeys/bitstring>>) ->
    RevHexKey = split_binary_to_hex(<<NodeKeys/bitstring>>),
    init_routing_table(RevHexKey, {self, self}).


route_key(Key, Table) ->
    route_key(<<Key/bitstring>>, Table, <<>>).

route_key(<<>>, _, <<UsedKeys/bitstring>>) ->
    {self, <<UsedKeys/bitstring>>};
route_key(<<Hex:4, Rest/bitstring>>, Table, <<UsedKeys/bitstring>>) ->
    case lists:keyfind(<<Hex:4>>, 1, Table) of
        false -> {{no_match, no_match}, <<UsedKeys/bitstring>>};
        {_, InnerList} when is_list(InnerList) -> route_key(<<Rest/bitstring>>, InnerList, <<UsedKeys/bitstring, Hex:4>>);
        {_, {self, self}}  -> {{self, self}, <<UsedKeys/bitstring, Hex:4>>};
        {_, NodeInfo}  -> {NodeInfo, <<UsedKeys/bitstring, Hex:4>>}
    end.



% Function to print the routing table
print_routing_table(Table) ->
    io:format("["),
    lists:foreach(fun({Key, Value}) ->
        case Value of
            List when is_list(List) ->
                io:format("{~p ,", [Key]),
                print_routing_table(List),
                io:format("} ,");
            _ -> io:format("{~p, ~p}", [Key, Value])
        end
    end, Table),
    io:format("]").


get_row(<<Hex:4, _/bitstring>>, Table, 0) ->
    [Tuple || Tuple = {E, _} <- Table, E =/= <<Hex:4>>];
get_row(<<Hex:4, Rest/bitstring>>, Table, I) ->
    case lists:keyfind(<<Hex:4>>, 1, Table) of
        {_, InnerList} when is_list(InnerList) -> get_row(<<Rest/bitstring>>, InnerList, I-1);
        {_, _} -> error;
        false -> error
    end.


rebuild_table(Table, [], _) ->
    Table;
rebuild_table(Table, [H|T], <<Hex:4, Rest/bitstring>>) ->
    rebuild_table([{<<Hex:4>>, Table} | H], T, <<Rest/bitstring>>).


modify_node(<<>>, _, Table, Acc, <<UsedKeys/bitstring>> , _) ->
    rebuild_table(Table, Acc, <<UsedKeys/bitstring>> );
modify_node(<<Hex:4, Rest/bitstring>>, NodeInfo, Table, Acc, <<UsedKeys/bitstring>> , Operation) ->
    case lists:keyfind(<<Hex:4>>, 1, Table) of
        false when Operation =:= add -> 
            NewTable = [{<<Hex:4>>, NodeInfo} | Table],
            rebuild_table(NewTable, Acc, <<UsedKeys/bitstring, Hex:4>>);
        {_, InnerList} when is_list(InnerList) ->
            Row = [Tuple || Tuple = {<<E:4/bitstring>> , _} <- Table, <<E:4/bitstring>> =/= <<Hex:4>>],
            modify_node(<<Rest/bitstring>>, NodeInfo, InnerList, [Row | Acc], <<UsedKeys/bitstring, Hex:4>> , Operation);
        {_, _} when Operation =:= remove -> 
            NewTable = [{<<K:4/bitstring>> , V} || {<<K:4/bitstring>> , V} <- Table, <<K:4/bitstring>> =/= <<Hex:4>>],
            rebuild_table(NewTable, Acc, <<UsedKeys/bitstring, Hex:4>>);
        _ -> Table
    end.
modify_node(<<Key/bitstring>>, NodeInfo, Table, Operation) ->
    modify_node(<<Key/bitstring>>, NodeInfo, Table, [], <<>>, Operation).


add_node({NodePid, NodeName}, Table) ->
    <<Key/bitstring>> = hash_name(NodeName),
    %add connection
    modify_node(<<Key/bitstring>>, {NodePid, NodeName}, Table, add).


update_routing(RoutingTable, Nodes) ->
    RoutingTable1 = lists:foldl(fun(OtherNode, RT) -> 
        add_node(OtherNode, RT)
    end, RoutingTable, Nodes),
    RoutingTable1.


get_all_routes(RoutingTable) ->
    get_all_routes(RoutingTable, []).

get_all_routes([], Acc) ->
    Acc;
get_all_routes([{_, InnerList} | Rest], Acc) when is_list(InnerList) ->
    get_all_routes(Rest, get_all_routes(InnerList, Acc));
get_all_routes([{_, {self, self}} | Rest], Acc) ->
    get_all_routes(Rest, Acc);
get_all_routes([{_, NodeInfo} | Rest], Acc) ->
    get_all_routes(Rest, [NodeInfo | Acc]).



remove_node({NodePid, NodeName}, Table) ->
    <<Key/bitstring>> = hash_name(NodeName),
    modify_node(<<Key/bitstring>>, {NodePid, NodeName}, Table, remove).

