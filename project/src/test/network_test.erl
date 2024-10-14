-module(network_test).

-import(network, [start_net/2, send_message/2]).

-include_lib("eunit/include/eunit.hrl").

network_test() ->
    % Start the first node in the current process
    erlang:set_cookie(node(), 'cookie'),
    register(list_to_atom("mailbox1"), self()),

    % Spawn a process to start the second node

    Node2 = spawn(fun() -> node_start({"mailbox1", "node1@localhost"}, {"mailbox2", "node1@localhost"}, 'cookie') end),
    receive
        {node_started, Node2} -> io:format("Second node started: ~p~n", [Node2]);
        Msg -> io:format("Unexpected msg: ~p~n", [Msg])
    after 5000 ->
        io:format("Timeout waiting for second node to start~n"),
        exit(timeout)
    end.



node_start(Origin, NodeName, Cookie) ->
    {ok, Node2} = start_net(NodeName, Cookie),
        % Send a message from the second node to the first node
    send_message(Origin, {node_started, Node2}).
