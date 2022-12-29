#!/usr/bin/env escript
-mode(compile).

-record(cave, {
    grid :: sets:set(pos()),
    max_y :: integer()
}).

-type pos() :: {X :: integer(), Y :: integer()}.

-define(SAND_FROM, ({500, 0})).

main(["part1"]) ->
    do(_HasBottom = false);
main(["part2"]) ->
    do(_HasBottom = true).

do(HasBottom) ->
    RockLines = get_rock_lines(),
    log("Got rock lines: ~p", [RockLines]),

    InitialCave = initial_cave(HasBottom, RockLines),
    log("Initial cave: ~p", [InitialCave]),

    ExtraCount = if HasBottom -> 1; not HasBottom -> 0 end,
    SandUnitsUntilFull = pour_sand_until_full(_From = ?SAND_FROM, InitialCave) + ExtraCount,
    log("Units of sand that pour until full: ~b", [SandUnitsUntilFull]).

get_rock_lines() ->
    case io:get_line("") of
        eof ->
            [];
        Line ->
            [parse_rock_lines(Line) | get_rock_lines()]
    end.

parse_rock_lines(Line) ->
    case io_lib:fread("~d,~d", Line) of
        {ok, [X, Y], " -> " ++ Rest} ->
            [{X, Y} | parse_rock_lines(Rest)];
        {ok, [X, Y], "\n"} ->
            [{X, Y}]
    end.

initial_cave(HasBottom, RockLines) ->
    {Grid, MaxY} = initial_cave_recur(RockLines, HasBottom, _GridAcc0 = [], _MaxY = 0),
    #cave{grid = Grid, max_y = MaxY}.

initial_cave_recur([RockLine | Next], HasBottom, GridAcc, MaxY) ->
    {UpdatedGridAcc, UpdatedMinY} = initial_cave_for_rock_line(RockLine, GridAcc, MaxY),
    initial_cave_recur(Next, HasBottom, UpdatedGridAcc, UpdatedMinY);
initial_cave_recur([], HasBottom, GridAcc, MaxY) ->
    {UpdatedGridAcc, UpdatedMaxY} = maybe_have_cave_bottom(HasBottom, GridAcc, MaxY),
    {sets:from_list(UpdatedGridAcc, [{version, 2}]), UpdatedMaxY}.

initial_cave_for_rock_line([From | [To | _] = Next], GridAcc, MaxY) ->
    {UpdatedGridAcc, UpdatedMinY} = draw_rock_line(From, To, GridAcc, MaxY),
    initial_cave_for_rock_line(Next, UpdatedGridAcc, UpdatedMinY);
initial_cave_for_rock_line([_], GridAcc, MaxY) ->
    {GridAcc, MaxY}.

maybe_have_cave_bottom(HasBottom, GridAcc, MaxY) ->
    case HasBottom of
        false ->
            {GridAcc, MaxY};
        true ->
            UpdatedMaxY = MaxY + 2,
            UpdatedGridAcc = [{X, UpdatedMaxY} || X <- lists:seq(-10000, 10000)] ++ GridAcc, % hackish but good enough
            {UpdatedGridAcc, UpdatedMaxY}
    end.

draw_rock_line({X, Y1}, {X, Y2}, GridAcc, MaxY) ->
    LineMinY = min(Y1, Y2),
    LineMaxY = max(Y1, Y2),
    UpdatedGridAcc = [{X, Y} || Y <- lists:seq(LineMinY, LineMaxY)] ++ GridAcc,
    {UpdatedGridAcc, max(MaxY, LineMaxY)};
draw_rock_line({X1, Y}, {X2, Y}, GridAcc, MaxY) ->
    LineMinX = min(X1, X2),
    LineMaxX = max(X1, X2),
    UpdatedGridAcc = [{X, Y} || X <- lists:seq(LineMinX, LineMaxX)] ++ GridAcc,
    {UpdatedGridAcc, max(MaxY, Y)}.

pour_sand_until_full(From, Cave) ->
    case pour_sand(From, Cave) of
        {resting, UpdatedCave} ->
            1 + pour_sand_until_full(From, UpdatedCave);
        full ->
            0
    end.

pour_sand({_SandX, SandY}, Cave)
  when SandY >= Cave#cave.max_y ->
    full;
pour_sand({SandX, SandY}, Cave) ->
    NewSandY = SandY + 1,
    case [X || X <- [SandX, SandX - 1, SandX + 1],
               not sets:is_element({X, NewSandY}, Cave#cave.grid)]
    of
        [NewSandX | _] ->
            pour_sand({NewSandX, NewSandY}, Cave);
        [] when {SandX, SandY} =:= ?SAND_FROM ->
            full;
        [] ->
            UpdatedGrid = sets:add_element({SandX, SandY}, Cave#cave.grid),
            {resting, Cave#cave{grid = UpdatedGrid}}
    end.

log(Format, Args) ->
    io:format(standard_error, Format ++ "\n", Args).
