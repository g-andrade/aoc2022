#!/usr/bin/env escript

main(["find_first_marker"]) ->
    Line = io:get_line(_Prompt = ""),
    StrLine = unicode:characters_to_list(Line),
    find_first_marker(StrLine, _Pos = 4).

find_first_marker([A, B, C, D | _], Pos)
  when A =/= B, A =/= C, A =/= D, B =/= C, B =/= D, C =/= D ->
    io:format("first marker: ~p~n", [Pos]);
find_first_marker([_ | Next], Pos) ->
    find_first_marker(Next, Pos + 1);
find_first_marker([], _Pos) ->
    io:format("didn't find first marker~n", []).
