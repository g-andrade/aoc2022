#!/usr/bin/env escript

main([KeepOrderArg]) ->
    KeepOrder = parse_keep_order_arg(KeepOrderArg),
    InitialStacks = read_initial_stacks(),
    log("initial stacks: ~p", [InitialStacks]),
    FinalStacks = read_and_process_moves(KeepOrder, InitialStacks),
    log("final stacks: ~p", [FinalStacks]),
    TopCrates = which_crate_tops_which_stack(FinalStacks),
    log("top crates: ~p", [TopCrates]),
    ok.

parse_keep_order_arg("reverse_order") ->
    reverse_order;
parse_keep_order_arg("keep_order") ->
    keep_order.

log(Format, Args) ->
    io:format(standard_error, Format ++ "\n", Args).

%% Initial Stacks
%%
read_initial_stacks() ->
    read_initial_stacks_recur(_Acc = #{}).

read_initial_stacks_recur(Acc) ->
    Line = io:get_line(""),
    case unicode:characters_to_binary(Line) of
        <<" 1", _/bytes>> ->
            _ = io:get_line(""), % empty line
            finish_reading_initial_stacks(Acc);
        LineBin ->
            UpdatedAcc = parse_initial_stacks_line(LineBin, Acc),
            read_initial_stacks_recur(UpdatedAcc)
    end.

parse_initial_stacks_line(LineBin, Acc) ->
    parse_initial_stacks_line_recur(LineBin, _StackNr = 1, Acc).

parse_initial_stacks_line_recur(LineBin, StackNr, Acc) ->
    case LineBin of
        <<"[", Crate, "] ", Next/bytes>> ->
            UpdatedAcc = place_crate_in_initial_stack(StackNr, Crate, Acc),
            parse_initial_stacks_line_recur(Next, StackNr + 1, UpdatedAcc);
        <<"[", Crate, "]\n">> ->
            place_crate_in_initial_stack(StackNr, Crate, Acc);
        <<"    ", Next/bytes>> ->
            parse_initial_stacks_line_recur(Next, StackNr + 1, Acc)
    end.

place_crate_in_initial_stack(StackNr, Crate, Acc) ->
    place_crate_in_stack(StackNr, Crate, Acc).

finish_reading_initial_stacks(Acc) ->
    maps:map(fun (_StackNr, StackAcc) -> lists:reverse(StackAcc) end,
             Acc).

%% Processing Moves
%%
read_and_process_moves(KeepOrder, Stacks) ->
    case io:fread(_Prompt = "", "move ~d from ~d to ~d") of
        Success when element(1, Success) =:= ok ->
            [MoveHowMany, SourceStackNr, DestinationStackNr] = element(2, Success),
            UpdatedStacks = process_moves(KeepOrder, MoveHowMany, SourceStackNr, DestinationStackNr, Stacks),
            read_and_process_moves(KeepOrder, UpdatedStacks);
        eof ->
            Stacks
    end.

process_moves(KeepOrder, MoveHowMany, SourceStackNr, DestinationStackNr, Stacks) ->
    case KeepOrder of
        reverse_order ->
            process_moves_in_rev_order(MoveHowMany, SourceStackNr, DestinationStackNr, Stacks);
        keep_order ->
            process_moves_in_order(MoveHowMany, SourceStackNr, DestinationStackNr, Stacks)
    end.


process_moves_in_rev_order(MoveHowMany, SourceStackNr, DestinationStackNr, Stacks0)
  when MoveHowMany > 0 ->
    log("processing: ~p", [Stacks0]),
    [Crate | RemainingSourceStack] = maps:get(SourceStackNr, Stacks0),
    Stacks1 = maps:update(SourceStackNr, RemainingSourceStack, Stacks0),
    Stacks2 = place_crate_in_stack(DestinationStackNr, Crate, Stacks1),
    process_moves_in_rev_order(MoveHowMany - 1, SourceStackNr, DestinationStackNr, Stacks2);
process_moves_in_rev_order(MoveHowMany, _SourceStackNr, _DestinationStackNr, Stacks)
  when MoveHowMany =:= 0 ->
    Stacks.

process_moves_in_order(MoveHowMany, SourceStackNr, DestinationStackNr, Stacks0) ->
    SourceStack = maps:get(SourceStackNr, Stacks0),
    DestinationStack = maps:get(DestinationStackNr, Stacks0, []),
    {Crates, RemainingSourceStack} = lists:split(MoveHowMany, SourceStack),
    EnlargedDestinationStack = Crates ++ DestinationStack,
    Stacks1 = maps:update(SourceStackNr, RemainingSourceStack, Stacks0),
    _Stacks2 = maps:put(DestinationStackNr, EnlargedDestinationStack, Stacks1).

place_crate_in_stack(StackNr, Crate, Stacks) ->
    maps:update_with(StackNr,
                     fun (Stack) -> [Crate | Stack] end,
                     [Crate],
                     Stacks).

%% Top Crates
%%
which_crate_tops_which_stack(Stacks) ->
    KvList = maps:to_list(Stacks),
    SortedKvList = lists:sort(KvList),
    lists:map(fun ({_StackNr, [TopCrate | _]}) -> TopCrate end,
              SortedKvList).
