-module(file_transfer).

-import(network, [send_message/2]).
-export([send_file/2, receive_file/1]).

-include_lib("kernel/include/file.hrl").

-define(CHUNK_SIZE, 1024).


% Function to send a file
send_file(Receiver, FilePath) ->
    case file:read_file_info(FilePath) of
        {ok, FileInfo} ->
            FileSize = FileInfo#file_info.size,
            ChunkSize = 1024, % Define the chunk size
            send_file_chunks(Receiver, FilePath, FileSize, ChunkSize, 0);
        {error, Reason} ->
            send_message(Receiver, {error, Reason})
    end.


send_file_chunks(Receiver, FilePath, FileSize, ChunkSize, Offset) when Offset < FileSize ->
    case file:pread(FilePath, Offset, ChunkSize) of
        {ok, Data} ->
            send_message(Receiver, {file_chunk, Data}),
            send_file_chunks(Receiver, FilePath, FileSize, ChunkSize, Offset + ChunkSize);
        {error, Reason} ->
            send_message(Receiver, {error, Reason})
    end;
send_file_chunks(Receiver, _FilePath, FileSize, _ChunkSize, Offset) when Offset >= FileSize ->
    send_message(Receiver, {file_transfer_complete}).


% Function to receive a file
receive_file(FilePath) ->
    receive_file_chunks(FilePath, []).

receive_file_chunks(FilePath, Chunks) ->
    receive
        {file_chunk, Data} ->
            receive_file_chunks(FilePath, [Data | Chunks]);
        {file_transfer_complete} ->
            file:write_file(FilePath, lists:reverse(Chunks));
        {error, Reason} ->
            {error, Reason}
    end.
