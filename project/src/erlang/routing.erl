-module(routing).

-import(utils, [replace_nth/3]).
-import(key_gen, [hash_name/1, binary_to_hex/1, hex_length/1, common_hex_prefix/2, get_column/2 ]).

-export([init_routing_table/1, route_key/2, print_routing_table/1, add_node/2, remove_node/2, get_row/2, update_routing/2, get_all_routes/1]).


init_routing_table(<<NodeKey/bitstring>>) ->
    NumRows = hex_length(NodeKey),
    Table = [ [] || _ <- lists:seq(1, NumRows)],
    {NodeKey, Table}.


route_key(Key, {Key, _Table}) ->
    {self, self};
route_key(Key, {TableKey, Table}) ->
    RowId = common_hex_prefix(Key, TableKey),
    ColumnId = get_column(Key, RowId),
    Row = lists:nth(RowId + 1, Table),

    case lists:keyfind(ColumnId, 1, Row) of
        false -> {no_match, no_match};
        {_, NodeInfo}  -> NodeInfo
    end.


print_routing_table({TableKey, Table}) ->
    io:format("NodeKey: ~p~n", [TableKey]),
    lists:foreach(fun(Row) -> io:format("~p~n", [Row]) end, Table).


get_row({TableKey, Table}, I) ->
    MaxRow = hex_length(TableKey),
    if
        I =< MaxRow ->
            Row = lists:nth(I + 1, Table),
            [Node || {_, Node} <- Row];
        true ->
            {error, {invalid_row, I}}
    end.


add_node({NodePid, NodeName}, {TableKey, Table}) ->
    <<Key/bitstring>> = hash_name(NodeName),
    
    case Key of
        TableKey ->
            {TableKey, Table};
        _ ->
            RowId = common_hex_prefix(Key, TableKey),
            ColumnId = get_column(Key, RowId),
            Row = lists:nth(RowId + 1, Table),
            
            case lists:keyfind(ColumnId, 1, Row) of
                {ColumnId, _} ->
                    {TableKey, Table};
                false ->
                    NewRow = [{ColumnId, {NodePid, NodeName}} | Row],
                    NewTable = replace_nth(RowId, Table, NewRow),
                    {TableKey, NewTable}
            end
    end.


remove_node({_NodePid, NodeName}, {TableKey, Table}) ->
    <<Key/bitstring>> = hash_name(NodeName),
    
    case Key of
        TableKey ->
            {TableKey, Table};
        _ ->
            RowId = common_hex_prefix(Key, TableKey),
            ColumnId = get_column(Key, RowId),
            Row = lists:nth(RowId + 1, Table),
            
            case lists:keyfind(ColumnId, 1, Row) of
                {ColumnId, _} ->
                    NewRow = lists:keydelete(ColumnId, 1, Row),
                    NewTable = replace_nth(RowId, Table, NewRow),
                    {TableKey, NewTable};
                false ->
                    {TableKey, Table}
            end
    end.



update_routing(RoutingTable, Nodes) ->
    RoutingTable1 = lists:foldl(fun(OtherNode, RT) -> 
        add_node(OtherNode, RT)
    end, RoutingTable, Nodes),
    RoutingTable1.


get_all_routes({_TableKey, Table}) ->
    [Node || Row <- Table, {_, Node} <- Row].


