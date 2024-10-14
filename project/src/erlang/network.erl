-module(network).
-export([start_net/2, send_message/2]).


start_net({MailBox, _Name}, Cookie) ->
    true = erlang:set_cookie(node(), Cookie),
    register(list_to_atom(MailBox), self()),
    {ok, self()}.


send_message({MailBox, Node}, Message) ->
    try
        {MailBox, Node} ! Message,
        io:format("Message sent to ~p@~p~n", [MailBox, Node]),
        ok
    catch
        _:Reason ->
            io:format("Failed to send message to ~p@~p: ~p~n", [MailBox, Node, Reason]),
            {error, Reason}
    end.