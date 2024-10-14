-module(key_gen_test).

-include_lib("eunit/include/eunit.hrl").

% Test cases for hash_name/1
hash_name_test() ->
    ?assertEqual(<<93,65,64,42,188,75,42,118,185,113,157,145,16,23,197,146>>, key_gen:hash_name("hello")),
    ?assertEqual(<<9,143,107,205,70,33,211,115,202,222,78,131,38,39,180,246>>, key_gen:hash_name("test")).

% Test cases for find_longest_shared_prefix/2
find_longest_shared_prefix_test() ->
    Key = <<"abcde">>,
    Ids = [<<"abcd">>, <<"cdf">>, <<"abcdef">>, <<"158f">>],
    ?assertEqual({<<"abcdef">>, {40, <<"abcde">>}}, key_gen:find_longest_shared_prefix(Key, Ids)).

% Test cases for shared_prefix/2
shared_prefix_test() ->
    ?assertEqual({2, <<0:2>>}, key_gen:shared_prefix(<<56:8, 91:8, 60:8>>, <<22:8, 69:8, 70:8>>)).

% Test cases for compare_prefix/4
compare_prefix_test() ->
    Key = <<56:8, 91:8, 60:8, 176:8, 107:8, 4:8, 22:8, 77:8, 133:8, 141:8, 54:8, 54:8, 5:8, 73:8, 98:8, 30:8>>,
    Id1 = <<22:8, 69:8, 70:8, 246:8, 2:8, 97:8, 199:8, 228:8, 190:8, 12:8, 95:8, 95:8, 154:8, 174:8, 236:8, 134:8>>,
    Id2 = <<120:8, 136:8, 42:8, 174:8, 176:8, 142:8, 154:8, 76:8, 129:8, 104:8, 123:8, 93:8, 226:8, 173:8, 215:8, 79:8>>,
    Id3 = <<19:8, 21:8, 224:8, 125:8, 197:8, 236:8, 253:8, 206:8, 195:8, 159:8, 84:8, 236:8, 22:8, 245:8, 100:8, 183:8>>,
    BestId = <<>>,
    BestPrefix = {0, <<>>},

    ?assertEqual({Id1, key_gen:shared_prefix(Key, Id1)}, key_gen:compare_prefix(Key, Id1, BestId, BestPrefix)),
    ?assertEqual({Id1, key_gen:shared_prefix(Key, Id1)}, key_gen:compare_prefix(Key, Id2, Id1, key_gen:shared_prefix(Key, Id1))),
    ?assertEqual({Id3, key_gen:shared_prefix(Key, Id3)}, key_gen:compare_prefix(Key, Id3, Id1, key_gen:shared_prefix(Key, Id1))).

% Test cases for compare_prefix/4 with different inputs
compare_prefix_different_test() ->
    Key = <<"abc">>,
    Id1 = <<"abcd">>,
    Id2 = <<"ab">>,
    BestId = <<>>,
    BestPrefix = {0, <<>>},
    ?assertEqual({Id1, key_gen:shared_prefix(Key, Id1)}, key_gen:compare_prefix(Key, Id1, BestId, BestPrefix)),
    ?assertEqual({Id1, key_gen:shared_prefix(Key, Id1)}, key_gen:compare_prefix(Key, Id2, Id1, key_gen:shared_prefix(Key, Id1))).


% Test cases for find_closest_id/2
find_closest_id_test() ->
    Key = <<56:8, 91:8, 60:8, 176:8, 107:8, 4:8, 22:8, 77:8, 133:8, 141:8, 54:8, 54:8, 5:8, 73:8, 98:8, 30:8>>,
    Id1 = <<22:8, 69:8, 70:8, 246:8, 2:8, 97:8, 199:8, 228:8, 190:8, 12:8, 95:8, 95:8, 154:8, 174:8, 236:8, 134:8>>,
    Id2 = <<120:8, 136:8, 42:8, 174:8, 176:8, 142:8, 154:8, 76:8, 129:8, 104:8, 123:8, 93:8, 226:8, 173:8, 215:8, 79:8>>,
    Id3 = <<19:8, 21:8, 224:8, 125:8, 197:8, 236:8, 253:8, 206:8, 195:8, 159:8, 84:8, 236:8, 22:8, 245:8, 100:8, 183:8>>,
    Ids = [Id1, Id2, Id3],
    {BestId, BestDiff} = key_gen:find_closest_id(Key, Ids),
    ?assertEqual({Id1, key_gen:find_difference(Id1, Key)}, {BestId, BestDiff}).

% Test cases for find_closest/3
find_closest_test() ->
    Key = <<56:8, 91:8, 60:8, 176:8, 107:8, 4:8, 22:8, 77:8, 133:8, 141:8, 54:8, 54:8, 5:8, 73:8, 98:8, 30:8>>,
    Id1 = <<22:8, 69:8, 70:8, 246:8, 2:8, 97:8, 199:8, 228:8, 190:8, 12:8, 95:8, 95:8, 154:8, 174:8, 236:8, 134:8>>,
    Id2 = <<120:8, 136:8, 42:8, 174:8, 176:8, 142:8, 154:8, 76:8, 129:8, 104:8, 123:8, 93:8, 226:8, 173:8, 215:8, 79:8>>,
    Id3 = <<19:8, 21:8, 224:8, 125:8, 197:8, 236:8, 253:8, 206:8, 195:8, 159:8, 84:8, 236:8, 22:8, 245:8, 100:8, 183:8>>,
    BestId = <<>>,
    Diff = 0,
    
    % Initial comparison
    ?assertEqual({Id1, key_gen:find_difference(Id1, Key)}, key_gen:find_closest(Key, Id1, BestId, Diff)),
    
    % Comparing with the next ID
    {BestId1, Diff1} = key_gen:find_closest(Key, Id1, BestId, Diff),
    ?assertEqual({Id1, key_gen:find_difference(Id1, Key)}, key_gen:find_closest(Key, Id2, BestId1, Diff1)),
    
    % Comparing with the next ID
    {BestId2, Diff2} = key_gen:find_closest(Key, Id2, BestId1, Diff1),
    ?assertEqual({Id1, key_gen:find_difference(Id1, Key)}, key_gen:find_closest(Key, Id3, BestId2, Diff2)).


