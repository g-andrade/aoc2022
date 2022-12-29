#!/usr/bin/env escript
-mode(compile).

-record(cave, {
    grid :: sets:set(pos()),
    max_y :: integer()
}).

-type pos() :: {X :: integer(), Y :: integer()}.

main(["part1"]) ->
    do_part1().

do_part1() ->
    RockLines = get_rock_lines(),
    log("Got rock lines: ~p", [RockLines]),

    InitialCave = initial_cave(RockLines),
    log("Initial cave: ~p", [InitialCave]),

    SandUnitsUntilFull = pour_sand_until_full(_From = {500,0}, InitialCave),
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

initial_cave(RockLines) ->
    {Grid, MaxY} = initial_cave_recur(RockLines, _GridAcc0 = [], _MaxY = 0),
    #cave{grid = Grid, max_y = MaxY}.

initial_cave_recur([RockLine | Next], GridAcc, MaxY) ->
    {UpdatedGridAcc, UpdatedMinY} = initial_cave_for_rock_line(RockLine, GridAcc, MaxY),
    initial_cave_recur(Next, UpdatedGridAcc, UpdatedMinY);
initial_cave_recur([], GridAcc, MaxY) ->
    {sets:from_list(GridAcc, [{version, 2}]), MaxY}.

initial_cave_for_rock_line([From | [To | _] = Next], GridAcc, MaxY) ->
    {UpdatedGridAcc, UpdatedMinY} = draw_rock_line(From, To, GridAcc, MaxY),
    initial_cave_for_rock_line(Next, UpdatedGridAcc, UpdatedMinY);
initial_cave_for_rock_line([_], GridAcc, MaxY) ->
    {GridAcc, MaxY}.

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
        [] ->
            UpdatedGrid = sets:add_element({SandX, SandY}, Cave#cave.grid),
            {resting, Cave#cave{grid = UpdatedGrid}}
    end.

log(Format, Args) ->
    io:format(standard_error, Format ++ "\n", Args).
