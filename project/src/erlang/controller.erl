-module(controller).
-export([start/3, start/4]).
-import(node, [start_node/2, start_node/3]).

start(ControllerName, N, NodeName) ->
    start(ControllerName, N, NodeName, "self").

start(ControllerName, N, NodeName, JoinAddr) ->
    JoinAddAtom = list_to_atom(JoinAddr),
    Nodes = spawn_nodes(ControllerName, N, NodeName, [], JoinAddAtom),
    loop(ControllerName, NodeName, Nodes, N).

spawn_nodes(_, 0, _NodeName, Nodes, _) -> Nodes;
spawn_nodes(ControllerName, N, NodeName, Nodes, JoinAddr) ->
    NodeAddr = list_to_atom(NodeName),
    NodeId = length(Nodes) + 1,
    Node = "node" ++ integer_to_list(NodeId),
    create_files(ControllerName, Node),
    case {JoinAddr, NodeId} of
        {self, 1} -> Pid = start_node(Node, NodeName);
        {self, _} -> Pid = start_node(Node, NodeName, {{node1, NodeAddr}, "node1"});
        {_ , _} -> Pid = start_node(Node, NodeName, {{node1, JoinAddr}, "node1"})
    end,
    spawn_nodes(ControllerName, N - 1, NodeName, [{Node, Pid} | Nodes], JoinAddr).

loop(ControllerName, NodeName, Nodes, LastId) ->
    io:format("Controller started. Enter commands:\n", []),
    loop(ControllerName, NodeName, Nodes, LastId, []).

loop(ControllerName, NodeName, Nodes, LastId, Buffer) ->
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
                            create_files(ControllerName, Node),
                            NodeAddr = list_to_atom(NodeName),
                            Pid = start_node(Node, NodeName, {{node1, NodeAddr}, "node1"}),
                            loop(ControllerName, NodeName, [{Node, Pid} | Nodes], NewLastId, Buffer);
                        _ ->
                            loop(ControllerName, NodeName, Nodes, LastId, Buffer)
                    end;

                ["spawn", Starter] ->
                    NewLastId = LastId + 1,
                    Node = "node" ++ integer_to_list(NewLastId),
                    case lists:keyfind(Node, 1, Nodes) of
                        false ->
                            create_files(ControllerName, Node),
                            Pid = start_node(Node, NodeName, Starter),
                            loop(ControllerName, NodeName, [{Node, Pid} | Nodes], NewLastId, Buffer);
                        _ ->
                            loop(ControllerName, NodeName, Nodes, LastId, Buffer)
                    end;

                ["spawn", Node, Starter] ->
                    case lists:keyfind(Node, 1, Nodes) of
                        false ->
                            create_files(ControllerName, Node),
                            Pid = start_node(Node, NodeName, Starter),
                            loop(ControllerName, NodeName, [{Node, Pid} | Nodes], LastId, Buffer);
                        _ ->
                            loop(ControllerName, NodeName, Nodes, LastId, Buffer)
                    end;

                ["kill_node", Node] ->
                    case lists:keyfind(Node, 1, Nodes) of
                        {Node, Pid} ->
                            Pid ! kill_node,
                            loop(ControllerName, NodeName, lists:keydelete(Node, 1, Nodes), LastId, Buffer);
                        false ->
                            loop(ControllerName, NodeName, Nodes, LastId, Buffer)
                    end;

                ["off"] ->
                    ok;

                _ ->
                    loop(ControllerName, NodeName, Nodes, LastId, Buffer)
            end
    end.


create_files(ControllerName, Node) ->
    File1 = "./files/" ++ Node ++ "/" ++ ControllerName ++ "_" ++ Node ++ "_file1.txt",
    File2 = "./files/" ++ Node ++ "/" ++ ControllerName ++ "_" ++ Node ++ "_file2.txt",
    ok = filelib:ensure_dir(File1),
    ok = filelib:ensure_dir(File2),
    Content1 = lists:duplicate(100, Node ++ "file1.txt\n"),
    ok = file:write_file(File1, Content1),
    Content2 = lists:duplicate(100, Node ++ "file2.txt\n"),
    ok = file:write_file(File2, Content2).