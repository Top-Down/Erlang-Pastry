-module(utils).
-export([get_time/0, replace_nth/3]).

get_time() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

replace_nth(N, List, NewElem) ->
    {Left, [_|Right]} = lists:split(N, List),
    Left ++ [NewElem] ++ Right.