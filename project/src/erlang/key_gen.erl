-module(key_gen).

-export([hash_name/1, find_longest_shared_prefix/2, shared_prefix/2, compare_prefix/4, find_closest_id/2, 
    find_difference/2, find_closest/4, common_hex_prefix/2, hex_length/1]).

% Function to hash a single file name
hash_name(Name) ->
    Hash = crypto:hash(md5, Name),
    <<Hash/bitstring>>.

% Function to find the shared prefix of two bitstrings
shared_prefix(Key1, Key2) ->
    shared_prefix(Key1, Key2, <<>>, 0).

shared_prefix(<<>>, _, Acc, Length) -> {Length, Acc};
shared_prefix(_, <<>>, Acc, Length) -> {Length, Acc};
shared_prefix(<<H1:1, T1/bitstring>>, <<H2:1, T2/bitstring>>, Acc, Length) when H1 == H2 ->
    shared_prefix(T1, T2, <<Acc/bitstring, H1:1>>, Length + 1);
shared_prefix(_, _, Acc, Length) -> {Length, Acc}.


common_hex_prefix(Bin1, Bin2) ->
    {BitLength, _} = shared_prefix(Bin1, Bin2, <<>>, 0),
    BitLength div 4.


hex_length(Bitstring) ->
    BitLength = bit_size(Bitstring),
    BitLength div 4.



find_closest_id(Key, Ids) ->
    lists:foldl(fun(Id, {BestId, BestDiff}) -> find_closest(Key, Id, BestId, BestDiff) end, {<<>>, 0}, Ids).

find_closest(Key, Id, BestId, BestDiff) ->
    Diff = find_difference(Key, Id),
    case (BestId == <<>>) orelse (Diff < BestDiff) orelse (Diff == BestDiff andalso Id < BestId) of
        true -> 
            {Id, Diff};
        false -> 
            {BestId, BestDiff}
    end.


find_difference(Key1, Key2) ->
    Integer1 = binary:decode_unsigned(Key1),
    Integer2 = binary:decode_unsigned(Key2),
    abs(Integer1 - Integer2).


compare_prefix(Key, Id, BestId, {BestLength, BestPrefix}) ->
    {Length, Prefix} = shared_prefix(Key, Id),
    case (BestId == <<>>) orelse (Length > BestLength) orelse (Length == BestLength andalso Id < BestId) of
        true -> 
            {Id, {Length, Prefix}};
        false -> 
            {BestId, {BestLength, BestPrefix}}
    end.

% Function to find the ID with the longest shared prefix
find_longest_shared_prefix(Key, Ids) ->
    lists:foldl(fun(Id, {BestId, BestPrefix}) -> compare_prefix(Key, Id, BestId, BestPrefix) end, {<<>>, {0, <<>>}}, Ids).
