-module(file_handler).

-import(network, [send_message/2]).
-export([store_file/3, get_file_size/1, delete_file/1, list_files/1, move_file/2]).

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
            ok;
        Other ->
            Other
    end.


store_file(FilePath, Size, FileData) ->
    case filelib:ensure_dir(FilePath) of
        ok ->
            case erlang:size(FileData) of
                Size ->
                    file:write_file(FilePath, FileData),
                    ok;
                _ ->
                    {error, size_mismatch}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


move_file(SrcPath, DestPath) ->
    case filelib:ensure_dir(DestPath) of
        ok ->
            case file:read_file(SrcPath) of
                {ok, FileData} ->
                    case file:write_file(DestPath, FileData) of
                        ok ->
                            file:delete(SrcPath),
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


list_files(Path) ->
    case file:list_dir(Path) of
        {ok, Items} ->
            Files = [Item || Item <- Items, filelib:is_regular(filename:join(Path, Item))],
            Files;
        _ -> []
    end.


