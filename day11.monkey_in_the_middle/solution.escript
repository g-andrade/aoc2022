#!/usr/bin/env escript
-mode(compile).

-record(monkey, {
    nr :: integer(),
    items :: queue:queue(worry_level()),
    operation :: operation_fun(),
    divisible_by :: pos_integer(),
    throw_to_monkey_nr_if_true :: integer(),
    throw_to_monkey_nr_if_false :: integer()
}).

-record(tweaks, {
    bored_divide_by :: pos_integer(),
    common_divisors_multiplied :: pos_integer()
}).

-type worry_level() :: integer().
-type operation_fun() :: fun ((worry_level()) -> worry_level()).

main(["part1"]) ->
    do(_NrOfRounds = 20, _BoredDivideBy = 3);
main(["part2"]) ->
    do(_NrOfRounds = 10_000, _BoredDivideBy = 1).

do(NrOfRounds, BoredDivideBy) ->
    Input = read_input(),
    InitialMonkeys = parse_monkeys(Input),
    log("Initial monkeys: ~p", [InitialMonkeys]),

    CommonDivisors = [Monkey#monkey.divisible_by || Monkey <- maps:values(InitialMonkeys)],
    CommonDivisorsMultiplied = lists:foldl(fun erlang:'*'/2, 1, CommonDivisors),
    Tweaks = #tweaks{bored_divide_by = BoredDivideBy,
                     common_divisors_multiplied = CommonDivisorsMultiplied},

    {InspectionCounts, FinalMonkeys}
        = do_rounds(
            fun (MonkeyNr, Acc) ->
                    update_counter(MonkeyNr, +1, Acc)
            end,
            _InspectionCountsAcc0 = #{},
            Tweaks,
            NrOfRounds,
            InitialMonkeys),

    log("Final monkeys: ~p", [FinalMonkeys]),
    log("Inspection counters: ~p", [InspectionCounts]),

    TopCounters = get_top_counters(_HowMany = 2, InspectionCounts),
    log("Top counters: ~p", [TopCounters]),

    MonkeyBusiness = lists:foldl(fun erlang:'*'/2, 1, [V || {_K, V} <- TopCounters]),
    log("Level of monkey business: ~b", [MonkeyBusiness]).

read_input() ->
    read_input_recur(_Acc0 = []).

read_input_recur(Acc) ->
    case io:get_line("") of
        Line when is_list(Line) ->
            read_input_recur([Acc, Line]);
        eof ->
            unicode:characters_to_binary(Acc)
    end.

parse_monkeys(Input) ->
    Blocks = binary:split(Input, [<<"\n\n">>], [global, trim]),
    ParsedMonkeys = lists:map(fun parse_monkey/1, Blocks),
    Enumerated = [{Monkey#monkey.nr, Monkey} || Monkey <- ParsedMonkeys],
    maps:from_list(Enumerated).

parse_monkey(Text) ->
    % log("parsing monkey from ~p", [Text]),

    {match, Match}
        = re:run(Text,
                 "Monkey (?<monkey_nr>[0-9]+):\n"
                 "  Starting items: (?<starting_items>.+)\n"
                 "  Operation: (?<operation>.+)\n"
                 "  Test: divisible by (?<divisible_by>[0-9]+)\n"
                 "    If true: throw to monkey (?<if_true_monkey_nr>[0-9]+)\n"
                 "    If false: throw to monkey (?<if_false_monkey_nr>[0-9]+)"
                 ,
                 [{capture, [<<"monkey_nr">>,
                             <<"starting_items">>,
                             <<"operation">>,
                             <<"divisible_by">>,
                             <<"if_true_monkey_nr">>,
                             <<"if_false_monkey_nr">>
                  ], binary}]),

    [RawMonkeyNr, RawStartingItems, RawOperation, RawDivisibleBy,
     RawIfTrueMonkeyNr, RawIfFalseMonkeyNr] = Match,

    #monkey{nr = binary_to_integer(RawMonkeyNr),
            items = parse_starting_items(RawStartingItems),
            operation = parse_operation(RawOperation),
            divisible_by = binary_to_integer(RawDivisibleBy),
            throw_to_monkey_nr_if_true = binary_to_integer(RawIfTrueMonkeyNr),
            throw_to_monkey_nr_if_false = binary_to_integer(RawIfFalseMonkeyNr)}.

parse_starting_items(RawStartingItems) ->
    BinWorryLevels = binary:split(RawStartingItems, [<<" ">>, <<",">>], [global, trim_all]),
    queue:from_list(lists:map(fun binary_to_integer/1, BinWorryLevels)).

parse_operation(<<"new = ", Expression/bytes>>) ->
    case binary:split(Expression, <<" ">>, [global, trim]) of
        [<<"old">>, RawOperator, <<"old">>] ->
            OperatorFun = parse_operator(RawOperator),
            fun (WorryLevel) -> apply(OperatorFun, [WorryLevel, WorryLevel]) end;
        [<<"old">>, RawOperator, BinNumArg] ->
            OperatorFun = parse_operator(RawOperator),
            NumArg = binary_to_integer(BinNumArg),
            fun (WorryLevel) -> apply(OperatorFun, [WorryLevel, NumArg]) end
    end.

parse_operator(RawOperation) ->
    maps:get(RawOperation, #{<<"+">> => fun erlang:'+'/2,
                             <<"-">> => fun erlang:'-'/2,
                             <<"*">> => fun erlang:'*'/2,
                             <<"/">> => fun erlang:'div'/2}).

do_rounds(Fun, Acc, Tweaks, Amount, Monkeys) when Amount > 0 ->
    % log("~b round(s) left: ~p", [Amount, Monkeys]),
    log("~b round(s) left", [Amount]),
    {UpdatedAcc, UpdatedMonkeys} = do_round(Fun, Acc, Tweaks, Monkeys),
    do_rounds(Fun, UpdatedAcc, Tweaks, Amount - 1, UpdatedMonkeys);
do_rounds(_Fun, Acc, _Tweaks, Amount, Monkeys) when Amount =:= 0 ->
    {Acc, Monkeys}.

do_round(Fun, Acc, Tweaks, Monkeys) ->
    do_round_recur(Fun, Acc, Tweaks, _MonkeyNr = 0, Monkeys).

do_round_recur(Fun, Acc, Tweaks, MonkeyNr, Monkeys) when MonkeyNr < map_size(Monkeys) ->
    Monkey = maps:get(MonkeyNr, Monkeys),
    try
        WorryLevel = queue:head(Monkey#monkey.items),
        IncreasedWorryLevel = (Monkey#monkey.operation)(WorryLevel),
        BoredWorryLevel = IncreasedWorryLevel div Tweaks#tweaks.bored_divide_by,
        RestrictedWorryLevel = BoredWorryLevel rem Tweaks#tweaks.common_divisors_multiplied,
        ThrowToMonkey
            = case RestrictedWorryLevel rem Monkey#monkey.divisible_by of
                  0 -> Monkey#monkey.throw_to_monkey_nr_if_true;
                  _ -> Monkey#monkey.throw_to_monkey_nr_if_false
              end,
        MonkeysAfterThrow = throw_to_monkey(ThrowToMonkey, RestrictedWorryLevel, Monkeys),

        % log("Monkey ~b got worry level ~b, increased it to ~b, bored it became ~b,"
        %     " threw it to monkey ~b",
        %     [MonkeyNr, WorryLevel, IncreasedWorryLevel, BoredWorryLevel, ThrowToMonkey]),

        UpdatedMonkey = Monkey#monkey{items = queue:drop(Monkey#monkey.items)},
        UpdatedMonkeys = maps:update(MonkeyNr, UpdatedMonkey, MonkeysAfterThrow),
        UpdatedAcc = Fun(MonkeyNr, Acc),
        do_round_recur(Fun, UpdatedAcc, Tweaks, MonkeyNr, UpdatedMonkeys)
    catch
        error:empty ->
            do_round_recur(Fun, Acc, Tweaks, MonkeyNr + 1, Monkeys)
    end;
do_round_recur(_Fun, Acc, _Tweaks, MonkeyNr, Monkeys) when MonkeyNr =:= map_size(Monkeys) ->
    {Acc, Monkeys}.

throw_to_monkey(ThrowToMonkey, BoredWorryLevel, Monkeys) ->
    Monkey = maps:get(ThrowToMonkey, Monkeys),
    UpdatedMonkey = Monkey#monkey{items = queue:in(BoredWorryLevel, Monkey#monkey.items)},
    maps:update(ThrowToMonkey, UpdatedMonkey, Monkeys).

update_counter(Key, Delta, Map) ->
    maps:update_with(Key,
                     fun (V) -> V + Delta end,
                     Delta,
                     Map).

get_top_counters(HowMany, Map) ->
    AsList = maps:to_list(Map),
    Sorted = lists:keysort(2, AsList),
    lists:sublist(Sorted, length(Sorted) - HowMany + 1, HowMany).

log(Format, Args) ->
    io:format(standard_error, Format ++ "\n", Args).
