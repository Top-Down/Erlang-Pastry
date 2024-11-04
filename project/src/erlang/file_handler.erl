-module(file_handler).

-import(network, [send_message/2]).
-export([send_file/2, receive_file/2, get_file_size/1, delete_file/1, list_files/1]).

-include_lib("kernel/include/file.hrl").

-define(CHUNK_SIZE, 1024).


get_file_size(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo} ->
            {ok, FileInfo#file_info.size};
        {error, Reason} ->
            {error, Reason}
    end.



delete_file(FilePath) ->
    case file:delete(FilePath) of
        ok ->
            io:format("File ~s deleted successfully.~n", [FilePath]);
        {error, Reason} ->
            io:format("Failed to delete file: ~p~n", [Reason])
    end.


send_file(Receiver, FilePath) ->
    case file:read_file(FilePath) of
        {ok, Data} ->
            send_message(Receiver, {file_data, Data});
        {error, Reason} ->
            send_message(Receiver, {error, Reason})
    end.


receive_file(FilePath, Size) ->
    receive
        {file_data, Data} ->
            case erlang:size(Data) of
                Size ->
                    file:write_file(FilePath, Data),
                    ok;
                _ ->
                    {error, size_mismatch}
            end;
        {error, _Reason} ->
            error
    after 3000 ->
        {error, timeout}
    end.


list_files(Path) ->
    {ok, Items} = file:list_dir(Path),
    Items.


