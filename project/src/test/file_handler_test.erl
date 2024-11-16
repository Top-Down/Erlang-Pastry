-module(file_handler_test).

-import(file_handler, [store_file/3, get_file_size/1, delete_file/1, list_files/1, move_file/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CHUNK_SIZE, 1024).

send_file_test() ->
    % Start node1

    FilePath = "files/lorem.txt",
    DestPath1 = "files1/lorem.txt",
    DestPath2 = "files2/lorem.txt",


    case file:read_file(FilePath) of
        {ok, FileData} ->
            Size = erlang:size(FileData),
            store_file(DestPath1, Size, FileData);
        {error, _Reason} ->
            ?assert(false)
    end,

    
    move_file(DestPath1, DestPath2).