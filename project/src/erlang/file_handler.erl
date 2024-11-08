-module(file_handler).

-import(network, [send_message/2]).
-export([store_file/3, get_file_size/1, delete_file/1, list_files/1]).

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


store_file(FilePath, FileData, Size) ->
    case erlang:size(FileData) of
        Size ->
            file:write_file(FilePath, FileData),
            ok;
        _ ->
            {error, size_mismatch}
    end.


list_files(Path) ->
    {ok, Items} = file:list_dir(Path),
    Items.


