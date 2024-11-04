-module(file_handler_test).

-import(file_handler, [send_file/2, receive_file/2, get_file_size/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CHUNK_SIZE, 1024).

send_file_test() ->
    % Start node1
    Father = self(),
    Node1 = spawn(fun() -> node_start("node1", Father) end),

    io:format("node1: ~p~n", [Node1]),
    timer:sleep(300),
    {_Res, Size} = get_file_size("files/sborra.txt"),
    Node1 ! {file_send, Size},
    send_file(Node1, "files/sborra.txt"),

    Pids = [Node1],
    receive_file_response(),
    cleanup(Pids).



node_start(_NodeName, Father) ->
    receive
        {file_send, Size} ->
            receive_file("files/sborra2.txt", Size),
            Father ! {file_received}
    after 5000 ->
        io:format("Test failed.~n"),
        ?assert(false)
    end.   



receive_file_response() ->
    receive
        {file_received} ->
            io:format("File send and received~n");
        Response -> 
            io:format("Other response: ~p~n", [Response]),
            ?assert(false)
    after 5000 ->
        io:format("Test failed.~n"),
        ?assert(false)
    end.


cleanup(Pids) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
    ok.