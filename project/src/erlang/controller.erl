-module(controller).
-export([start/2]).
-import(node, [start_node/2, start_node/3]).

start(N, NodeName) ->
    Pid = spawn(fun() -> init(N, NodeName) end),
    register(controller, Pid).

init(N, NodeName) ->
    Nodes = spawn_nodes(N, NodeName, []),
    loop(NodeName, Nodes, N).


spawn_nodes(0, NodeName, Nodes) -> {NodeName, Nodes};
spawn_nodes(N, NodeName, Nodes) ->
    NodeAddr = list_to_atom(NodeName),
    NodeId = length(Nodes) + 1,
    Node = "node" ++ integer_to_list(NodeId),
    create_files(Node),
    case NodeId of
        1 -> Pid = start_node(Node, NodeName);
        _ -> Pid = start_node(Node, NodeName, {{node1, NodeAddr}, "node1"})
    end,
    spawn_nodes(N - 1, NodeName, [{Node, Pid} | Nodes]).


loop(NodeName, Nodes, LastId) ->
    receive
        {spawn} ->
            NewLastId = LastId + 1,
            Node = "node" ++ integer_to_list(NewLastId),
            case lists:keyfind(Node, 1, Nodes) of
                false ->
                    create_files(Node),
                    Pid = start_node(Node, NodeName, {{node1, NodeName}, "node1"}),
                    loop(NodeName, [{Node, Pid} | Nodes], NewLastId);
                _ ->
                    loop(NodeName, Nodes, LastId)
            end;

        {spawn, Starter} ->
            NewLastId = LastId + 1,
            Node = "node" ++ integer_to_list(NewLastId),
            case lists:keyfind(Node, 1, Nodes) of
                false ->
                    create_files(Node),
                    Pid = start_node(Node, NodeName, Starter),
                    loop(NodeName, [{Node, Pid} | Nodes], NewLastId);
                _ ->
                    loop(NodeName, Nodes, LastId)
            end;

        {spawn, Node, Starter} ->
            case lists:keyfind(Node, 1, Nodes) of
                false ->
                    create_files(Node),
                    Pid = start_node(Node, NodeName, Starter),
                    loop(NodeName, [{Node, Pid} | Nodes], LastId);
                _ ->
                    loop(NodeName, Nodes, LastId)
            end;

        {kill_node, Node} ->
            case lists:keyfind(Node, 1, Nodes) of
                {Node, Pid} ->
                    Pid ! kill_node,
                    loop(NodeName, lists:keydelete(Node, 1, Nodes), LastId);
                false ->
                    loop(NodeName, Nodes, LastId)
            end;

        off ->
            ok;

        _ ->
            loop(NodeName, Nodes, LastId)
    end.


create_files(Node) ->
    File1 = "./files/" ++ Node ++ "file1.txt",
    File2 = "./files/" ++ Node ++ "file2.txt",
    ok = filelib:ensure_dir(File1),
    ok = filelib:ensure_dir(File2),
    Content = lists:duplicate(100, Node ++ "file1.txt\n"),
    ok = file:write_file(File1, Content),
    Content = lists:duplicate(100, Node ++ "file2.txt\n"),
    ok = file:write_file(File2, Content).