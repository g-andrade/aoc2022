#!/usr/bin/env escript
-mode(compile).

main(["sum"]) ->
    do_sum();
main(["crt"]) ->
    do_crt().

do_sum() ->
    %
    % Part 1
    %
    TargetCycles = [20, 60, 100, 140, 180, 220],
    {[], Sum} = run(
        fun (CurrentCycle, X, {[CurrentCycle | NextTargets], Sum}) ->
                UpdatedSum = Sum + (CurrentCycle * X),
                log("X at ~bth cycle is ~b, updated sum is ~b", [CurrentCycle, X, Sum]),
                {NextTargets, UpdatedSum};
            (_, _, Acc) ->
                Acc
        end,
        _Acc0 = {TargetCycles, _Sum0 = 0}),

    io:format("The sum is ~b~n", [Sum]).

do_crt() ->
    %
    % Part 2
    %
    ReverseRendering = run(
      fun (CurrentCycle, X, PixelsAcc) ->
              SpritePosition = X,
              PixelX = ((CurrentCycle - 1) rem 40),
              log("Sprite position is ~p, PixelX is ~p", [SpritePosition, PixelX]),

              case SpritePosition + 1 >= PixelX
                   andalso SpritePosition - 1 =< PixelX
              of
                  true when PixelX =:= 39 ->
                      [$\n, $# | PixelsAcc];
                  true ->
                      [$# | PixelsAcc];
                  false when PixelX =:= 39 ->
                      [$\n, $\. | PixelsAcc];
                  false ->
                      [$\. | PixelsAcc]
              end
      end,
      _Acc0 = ""),

    io:format("CRT:~n~s", [lists:reverse(ReverseRendering)]).

run(Fun, Acc) ->
    run_recur(Fun, Acc, _CurrentCycle = 1, _X = 1).

run_recur(Fun, Acc, CurrentCycle, X) ->
    case io:get_line("") of
        "noop" ++ _ ->
            run_cycles_and_continue(Fun, Acc, CurrentCycle, X, [0]);
        "addx " ++ RestOfTheLine ->
            {ok, [Delta], _} = io_lib:fread("~d", RestOfTheLine),
            run_cycles_and_continue(Fun, Acc, CurrentCycle, X, [0, Delta]);
        eof ->
            Acc
    end.

run_cycles_and_continue(Fun, Acc, CurrentCycle, X, [Delta | NextSteps]) ->
    UpdatedAcc = Fun(CurrentCycle, X, Acc),
    run_cycles_and_continue(Fun, UpdatedAcc, CurrentCycle + 1, X + Delta, NextSteps);
run_cycles_and_continue(Fun, Acc, CurrentCycle, X, []) ->
    run_recur(Fun, Acc, CurrentCycle, X).

log(Format, Args) ->
    io:format(standard_error, Format ++ "\n", Args).
