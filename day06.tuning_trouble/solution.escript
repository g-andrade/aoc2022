#!/usr/bin/env escript
-mode(compile).

-record(multiset, {
    total_count :: integer(),
    count_per_element :: #{term() => pos_integer()}
}).

main([Cmd]) ->
    PatternLength = pattern_length(Cmd),
    Line = io:get_line(_Prompt = ""),
    StrLine = unicode:characters_to_list(Line),
    {StartingSetElements, Next} = lists:split(PatternLength, StrLine),
    WindowSeq = queue:from_list(StartingSetElements),
    WindowSet = multiset_from_list(StartingSetElements),
    find_marker(WindowSeq, WindowSet, Next, _Pos = PatternLength).

pattern_length("packet_marker") -> 4;
pattern_length("msg_marker") -> 14.

find_marker(WindowSeq, WindowSet, [X | Next], Pos) ->
    case queue:len(WindowSeq) =:= map_size(WindowSet#multiset.count_per_element) of
        true ->
            io:format("found marker: ~b~n", [Pos]);
        false ->
            {{value, NextToDrop}, WindowSeq2} = queue:out(WindowSeq),
            WindowSet2 = multiset_remove(NextToDrop, WindowSet),
            WindowSeq3 = queue:in(X, WindowSeq2),
            WindowSet3 = multiset_add(X, WindowSet2),
            find_marker(WindowSeq3, WindowSet3, Next, Pos + 1)
    end;
find_marker(_WindowSeq, _WindowSet, [], _Pos) ->
    io:format("didn't find marker~n", []).

multiset_from_list(List) ->
    Set = multiset_new(),
    lists:foldl(fun multiset_add/2, Set, List).

multiset_new() ->
    #multiset{total_count = 0, count_per_element = #{}}.

multiset_add(Element, #multiset{total_count = TotalCount,
                                count_per_element = CountPerElement} = Set) ->
    Set#multiset{
        total_count = TotalCount + 1,
        count_per_element
            = maps:update_with(Element,
                               fun (Count) -> Count + 1 end,
                               _Default = 1,
                               CountPerElement)
    }.

multiset_remove(Element, #multiset{total_count = TotalCount,
                                   count_per_element = CountPerElement} = Set) ->
    case maps:get(Element, CountPerElement) - 1 of
        0 ->
            Set#multiset{
                total_count = TotalCount - 1,
                count_per_element = maps:remove(Element, CountPerElement)
            };
        RemainingElementCount ->
            Set#multiset{
                total_count = TotalCount - 1,
                count_per_element = maps:update(Element, RemainingElementCount, CountPerElement)
            }
    end.
