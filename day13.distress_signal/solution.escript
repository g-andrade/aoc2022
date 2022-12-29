#!/usr/bin/env escript
-mode(compile).

main(["part1"]) ->
    do().

do() ->
    do_recur(_Index = 1, _Sum = 0).

do_recur(Index, Sum) ->
    case get_packets() of
        finished ->
            log("Sum: ~p", [Sum]);
        {PacketA, PacketB} ->
            % log("Got packets: ~p, ~p", [{PacketA}, {PacketB}]),
            UpdatedSum
                = case compare_packets(PacketA, PacketB) of
                      gt ->
                          Sum;
                      _ ->
                          % log("Packets ~b in order, prev sum ~b", [Index, Sum]),
                          Sum + Index
                  end,
            do_recur(Index + 1, UpdatedSum)
    end.

get_packets() ->
    case io:get_line("") of
        eof ->
            finished;
        "\n" ->
            PacketA = [_|_] = io:get_line(""),
            PacketB = [_|_] = io:get_line(""),
            {parse_packet(PacketA), parse_packet(PacketB)};
        PacketA ->
            PacketB = [_|_] = io:get_line(""),
            {parse_packet(PacketA), parse_packet(PacketB)}
    end.

parse_packet(Line) ->
    {ok, Tokens, _} = erl_scan:string(Line),
    TerminatedTokens = Tokens ++ [{dot, 1}],
    {ok, Packet} = erl_parse:parse_term(TerminatedTokens),
    Packet.

compare_packets(A, B)
  when is_integer(A), is_integer(B) ->
    case A - B of
        Diff when Diff < 0 ->
            lt;
        Diff when Diff > 0 ->
            gt;
        _ ->
            eq
    end;
compare_packets(A, B)
  when is_integer(A) ->
    compare_packets([A], B);
compare_packets(A, B)
  when is_integer(B) ->
    compare_packets(A, [B]);
compare_packets([A | NextA], [B | NextB]) ->
    case compare_packets(A, B) of
        eq ->
            compare_packets(NextA, NextB);
        Other ->
            Other
    end;
compare_packets([], []) ->
    eq;
compare_packets([], _) ->
    lt;
compare_packets(_, []) ->
    gt.

log(Format, Args) ->
    io:format(standard_error, Format ++ "\n", Args).
