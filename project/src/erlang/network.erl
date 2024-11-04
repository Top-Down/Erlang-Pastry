-module(network).

-export([start_net/3, send_message/2]).

start_net(MailBox, NodeName, Cookie) ->
    % Set the cookie only if it's not already set
    case erlang:get_cookie() of
        undefined -> erlang:set_cookie(node(), Cookie);
        _ -> ok
    end,

    MailBoxAtom = list_to_atom(MailBox),
    NodeAddr = list_to_atom(NodeName),
    SelfAddr = {MailBoxAtom, NodeAddr},
    % Register the mailbox and check if successful
    case register(MailBoxAtom, self()) of
        true -> {ok, SelfAddr};
        false -> {error, already_registered}
    end.


send_message(NodeAddr, Message) ->
    try
        NodeAddr ! Message,
        io:format("Message sent to ~p~n", [NodeAddr]),
        ok
    catch
        _:Reason ->
            io:format("Failed to send message to ~p: ~p~n", [NodeAddr, Reason]),
            {error, Reason}
    end.
