-module(node).

-export([start_node/2]).

get_time() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

start_node(Name, NodeName) ->
    Cookie = "pastry",
    case erlang:get_cookie() of
            undefined -> erlang:set_cookie(node(), Cookie);
            _ -> ok
        end,

    MailBoxAtom = list_to_atom(Name),
    NodeAddr = list_to_atom(NodeName),
    SelfAddr = {MailBoxAtom, NodeAddr},

    case register(MailBoxAtom, self()) of
        true -> node_loop({SelfAddr, Name});
        false -> {error, already_registered}
    end.

% Main loop for the provider node
node_loop(SelfInfo) ->
  receive
    {From, Msg_id, _Timestamp, {find, FileName}} ->
        io:format("Received find request for ~p~n", [FileName]),
        find(SelfInfo, From, Msg_id, FileName),
        node_loop(SelfInfo);

    {From, Msg_id, _Timestamp, {delete, FileName}} ->
        io:format("Received delete request for ~p~n", [FileName]),
        delete(SelfInfo, From, Msg_id, FileName),
        node_loop(SelfInfo);
    
    {From, Msg_id, _Timestamp, {store_find, FileName}} ->
        io:format("Received store_find request for ~p~n", [FileName]),
        find_store(SelfInfo, From, Msg_id, FileName),
        node_loop(SelfInfo);

    {From, Msg_id, _Timestamp, {store, FileName, FileSize, FileData}} ->
        io:format("Received store request for ~p of size ~p~n", [FileName, FileSize]),
        store(SelfInfo, From, Msg_id, FileName, FileSize, FileData),
        node_loop(SelfInfo);

    {From, Msg_id, _Timestamp, {get_all_files}} ->
        io:format("Received get_all_files request~n", []),
        get_all_files(SelfInfo, From, Msg_id),
        node_loop(SelfInfo);

    _ ->
      io:format("Received unknown request~n", []),
      node_loop(SelfInfo)
  end.

find(_SelfInfo, From, Msg_id, FileName) ->
    FilePath = "./files/" ++ FileName,
    case file:read_file(FilePath) of
        {ok, FileData} ->
            FileSize = byte_size(FileData),
            io:format("File ~p found with size ~p~n", [FileName, FileSize]),
            From ! {self(), Msg_id, get_time(), {find_end, FileName, FileSize, FileData}};
        {error, Reason} ->
            io:format("Error reading file ~p: ~p~n", [FileName, Reason]),
            From ! {self(), Msg_id, get_time(), {error, Reason}}
    end.

find_store({SelfAddr, _SelfName}, From, Msg_id, _FileName) ->
    io:format("Sending store_found response with SelfAddr ~p~n", [SelfAddr]),
    From ! {self(), Msg_id, get_time(), {store_found, SelfAddr}}.

store(_SelfInfo, From, Msg_id, FileName, _FileSize, FileData) ->
    FilePath = "./files/" ++ FileName,
    case file:write_file(FilePath, FileData) of
        ok ->
            io:format("File ~p stored successfully~n", [FileName]),
            From ! {self(), Msg_id, get_time(), {store_end}};
        {error, Reason} ->
            io:format("Error storing file ~p: ~p~n", [FileName, Reason]),
            From ! {self(), Msg_id, get_time(), {error, Reason}}
    end.

get_all_files(_SelfInfo, From, Msg_id) ->
    case file:list_dir("./files") of
        {ok, FileList} ->
            io:format("Listing all files: ~p~n", [FileList]),
            From ! {self(), Msg_id, get_time(), {all_files_res, FileList}};
        {error, Reason} ->
            io:format("Error listing files: ~p~n", [Reason]),
            From ! {self(), Msg_id, get_time(), {error, Reason}}
    end.

delete(_SelfInfo, From, Msg_id, FileName) ->
    FilePath = "./files/" ++ FileName,
    case file:delete(FilePath) of
        ok ->
            io:format("File ~p deleted successfully~n", [FileName]),
            From ! {self(), Msg_id, get_time(), {delete_end}};
        {error, Reason} ->
            io:format("Error deleting file ~p: ~p~n", [FileName, Reason]),
            From ! {self(), Msg_id, get_time(), {error, Reason}}
    end.