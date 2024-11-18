-module(controller).
-export([start/3, start/2]).
-import(node, [start_node/2, start_node/3]).

start(N, NodeName) ->
    start(N, NodeName, self).

start(N, NodeName, JoinAddr) ->
    Nodes = spawn_nodes(N, NodeName, [], JoinAddr),
    loop(NodeName, Nodes, N).

spawn_nodes(0, NodeName, Nodes, ) -> Nodes;
spawnnodes(N, NodeName, Nodes, JoinAddr) ->
    NodeAddr = list_to_atom(NodeName),
    NodeId = length(Nodes) + 1,
    Node = "node" ++ integer_to_list(NodeId),
    create_files(Node),
    case {JoinAddr, NodeId} of
        {self, 1} -> Pid = start_node(Node, NodeName);
        {self, } -> Pid = startnode(Node, NodeName, {{node1, NodeAddr}, "node1"});
        {, _} -> Pid = start_node(Node, NodeName, {{node1, JoinAddr}, "node1"})
    end,
    spawn_nodes(N - 1, NodeName, [{Node, Pid} | Nodes], JoinAddr).

loop(NodeName, Nodes, LastId) ->
    io:format("Controller started. Enter commands:\n", []),
    loop(NodeName, Nodes, LastId, []).

loop(NodeName, Nodes, LastId, Buffer) ->
    io:format("> ", []),
    case io:get_line("") of
        eof -> ok;
        {error, _} -> ok;
        Line ->
            case string:tokens(Line, " \t\n") of
                ["spawn"] ->
                    NewLastId = LastId + 1,
                    Node = "node" ++ integer_to_list(NewLastId),
                    case lists:keyfind(Node, 1, Nodes) of
                        false ->
                            create_files(Node),
                            NodeAddr = list_to_atom(NodeName),
                            Pid = start_node(Node, NodeName, {{node1, NodeAddr}, "node1"}),
                            loop(NodeName, [{Node, Pid} | Nodes], NewLastId, Buffer);
                        _ ->
                            loop(NodeName, Nodes, LastId, Buffer)
                    end;

                ["spawn", Starter] ->
                    NewLastId = LastId + 1,
                    Node = "node" ++ integer_to_list(NewLastId),
                    case lists:keyfind(Node, 1, Nodes) of
                        false ->
                            create_files(Node),
                            Pid = start_node(Node, NodeName, Starter),
                            loop(NodeName, [{Node, Pid} | Nodes], NewLastId, Buffer);
                        _ ->
                            loop(NodeName, Nodes, LastId, Buffer)
                    end;

                ["spawn", Node, Starter] ->
                    case lists:keyfind(Node, 1, Nodes) of
                        false ->
                            create_files(Node),
                            Pid = start_node(Node, NodeName, Starter),
                            loop(NodeName, [{Node, Pid} | Nodes], LastId, Buffer);
                        _ ->
                            loop(NodeName, Nodes, LastId, Buffer)
                    end;

                ["kill_node", Node] ->
                    case lists:keyfind(Node, 1, Nodes) of
                        {Node, Pid} ->
                            Pid ! kill_node,
                            loop(NodeName, lists:keydelete(Node, 1, Nodes), LastId, Buffer);
                        false ->
                            loop(NodeName, Nodes, LastId, Buffer)
                    end;

                ["off"] ->
                    ok;

                _ ->
                    loop(NodeName, Nodes, LastId, Buffer)
            end
    end.

create_files(Node) ->
    File1 = "./files/" ++ Node ++ "/" ++ Node ++ "file1.txt",
    File2 = "./files/" ++ Node ++ "/" ++ Node ++ "file2.txt",
    ok = filelib:ensure_dir(File1),
    ok = filelib:ensure_dir(File2),
    Content1 = lists:duplicate(100, Node ++ "file1.txt\n"),
    ok = file:write_file(File1, Content1),
    Content2 = lists:duplicate(100, Node ++ "file2.txt\n"),
    ok = file:write_file(File2, Content2).