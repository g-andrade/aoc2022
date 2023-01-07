#!/usr/bin/env escript
-mode(compile).

main(["part1"]) ->
    do_part1().

-record(valve, {id, frate, next, open}).

-record(search, {flow_parcels, total_flow, clock, current, all, rev_path, nr_of_valves_opened}).

-define(DURATION, 30).

do_part1() ->
    ParsedInput = parse_input(),
    {value, Start, OtherValvesList} = lists:keytake('AA', #valve.id, ParsedInput),
    log("Starting at ~p, next candidates are ~p", [Start, OtherValvesList]),
    UpdatedStart = Start#valve{open = true},
    Valves = maps:from_list([{Valve#valve.id, Valve}
                             || Valve <- [UpdatedStart | OtherValvesList]]),

    Search = #search{
        flow_parcels = 0,
        total_flow = 0,
        clock = 0,
        current = UpdatedStart#valve.id,
        all = Valves,
        rev_path = [UpdatedStart#valve.id],
        nr_of_valves_opened = 1
    },
    InitialPQ = pqueue_push(Search, pqueue_new()),
    MaxFlow = search_max_flow(_MaxFlowSoFar = 0, InitialPQ),
    log("Max flow: ~b", [MaxFlow]).

parse_input() ->
    case io:get_line("") of
        "Valve " ++ ValveSpec ->
            ValveSpecWithoutNewLine = lists:sublist(ValveSpec, length(ValveSpec) - 1),
            [parse_valve(ValveSpecWithoutNewLine) | parse_input()];
        _ ->
            []
    end.

parse_valve(ValveSpec) ->
    [IdA, IdB | Rest1] = ValveSpec,
    IdStr = [IdA, IdB],
    " has flow rate=" ++ Rest2 = Rest1,
    {ok, [FlowRate], Rest3} = io_lib:fread("~d", Rest2),
    Id = list_to_atom(IdStr),
    Open = (FlowRate =:= 0), % This helps speed things up
    case Rest3 of
        "; tunnels lead to valves " ++ Rest4 ->
            NextStrs = string:split(Rest4, ", ", all),
            Next = lists:map(fun list_to_atom/1, NextStrs),
            #valve{id = Id, frate = FlowRate, next = Next, open = Open};
        "; tunnel leads to valve " ++ SingleNextStr ->
            Next = [list_to_atom(SingleNextStr)],
            #valve{id = Id, frate = FlowRate, next = Next, open = Open}
    end.

search_max_flow(MaxFlowSoFar, PQ) ->
    case pqueue_pop(PQ) of
        {Search, RemainingPQ} ->
            RemainingPQSize = pqueue_size(RemainingPQ),
            {Possibilities, NewMaxFlowSoFar} = do_search(Search, MaxFlowSoFar, RemainingPQSize),
            UpdatedPQ = lists:foldl(fun pqueue_push/2, RemainingPQ, Possibilities),
            search_max_flow(NewMaxFlowSoFar, UpdatedPQ);
        none ->
            MaxFlowSoFar
    end.

do_search(Search, MaxFlowSoFar, PQSize)
  when Search#search.clock =:= ?DURATION ->
    %log("Reached end with t flow ~b, path ~p", [Search#search.total_flow,
    %                                            lists:reverse(Search#search.rev_path)]),
    MaybeBetter = max(MaxFlowSoFar, Search#search.total_flow),
    _ = MaybeBetter =/= MaxFlowSoFar
        andalso begin
                    log("New best [1]: ~b, queue size ~b", [MaybeBetter, PQSize])
                end,
    {[], MaybeBetter};
do_search(Search, MaxFlowSoFar, _PQSize)
  when Search#search.clock < ?DURATION,
       Search#search.nr_of_valves_opened < map_size(Search#search.all) ->
    case max_flow_potential(Search) of
        MaxFlowPotential when MaxFlowPotential < MaxFlowSoFar ->
            %log("~b < ~b, dropping ~p", [MaxFlowPotential, MaxFlowSoFar,
            %                             lists:reverse(Search#search.rev_path)]),
            {[], MaxFlowSoFar};
        _ ->
            CurrentValveId = Search#search.current,
            CurrentValve = maps:get(CurrentValveId, Search#search.all),
            NextCandidateIds = CurrentValve#valve.next,
            Possibilities = lists:flatten([decisions(NextCandidateId, Search)
                                           || NextCandidateId <- NextCandidateIds]),
            {Possibilities, MaxFlowSoFar}
    end;
do_search(Search, MaxFlowSoFar, PQSize)
  when Search#search.clock < ?DURATION,
       Search#search.nr_of_valves_opened =:= map_size(Search#search.all) ->
    % No more valves left to open
    MinutesLeft = ?DURATION - Search#search.clock,
    FlowAtTheEnd = Search#search.total_flow + (MinutesLeft * Search#search.flow_parcels),
    MaybeBetter = max(MaxFlowSoFar, FlowAtTheEnd),
    _ = MaybeBetter =/= MaxFlowSoFar
        andalso begin
                    log("New best [2]: ~b, queue size ~b", [MaybeBetter, PQSize])
                end,
    {[], MaybeBetter}.

pqueue_new() ->
    {gb_trees:empty(), 0}.

pqueue_push(Search, {PQ, Size}) ->
    % Priority = {Search#search.clock, Search#search.total_flow, Search#search.flow_parcels},
    % Priority = {Search#search.total_flow, Search#search.flow_parcels, Search#search.clock},
    % Priority = [Search#search.total_flow|max_flow_potential(Search)],
    Priority = Search#search.total_flow,

    try gb_trees:get(Priority, PQ) of
        Q ->
            UpdatedQ = queue:in(Search, Q),
            {gb_trees:update(Priority, UpdatedQ, PQ), Size + 1}
    catch
        error:function_clause ->
            NewQ = queue:from_list([Search]),
            {gb_trees:insert(Priority, NewQ, PQ), Size + 1}
    end.

pqueue_pop({PQ, Size}) ->
    try gb_trees:largest(PQ) of
        {Priority, Q} ->
            {{value, Search}, RemainingQ} = queue:out(Q),
            case queue:is_empty(RemainingQ) of
                false ->
                    UpdatedPQ = gb_trees:update(Priority, RemainingQ, PQ),
                    {Search, {UpdatedPQ, Size - 1}};
                true ->
                    {_, _, UpdatedPQ} = gb_trees:take_largest(PQ),
                    {Search, {UpdatedPQ, Size - 1}}
            end
    catch
        error:function_clause ->
            none
    end.

% pqueue_to_list({PQ, _Size}) ->
%     [{Priority, queue:to_list(Q)} || {Priority, Q} <- gb_trees:to_list(PQ)].

pqueue_size({_PQ, Size}) ->
    Size.

decisions(NextCandidateId, Search) ->
    NextCandidate = maps:get(NextCandidateId, Search#search.all),

    case not NextCandidate#valve.open andalso Search#search.clock =< ?DURATION - 2 of
        true ->
            [open_decision(NextCandidate, Search),
             walk_by_decision(NextCandidate, Search)];
        _ ->
            walk_by_decision(NextCandidate, Search)
    end.

max_flow_potential(Search) ->
    UnopenedValves = [V || V <- maps:values(Search#search.all), not V#valve.open],
    NrOfUnopenedValves = length(UnopenedValves),
    Sorted = lists:keysort(#valve.frate, UnopenedValves),

    MinutesLeft = ?DURATION - Search#search.clock,
    MaxUnopenedValves = min(MinutesLeft div 2, length(UnopenedValves)),
    MinutesLeftAfterOpeningValves = MinutesLeft - (MaxUnopenedValves * 2),
    CutOff = lists:sublist(Sorted, NrOfUnopenedValves - MaxUnopenedValves + 1, MaxUnopenedValves),
    FRates = [V#valve.frate || V <- lists:reverse(CutOff)],

    Search#search.total_flow
    + (Search#search.flow_parcels * MinutesLeft)
    + calc_max_extra_flow_potential(FRates, _AccParcels = 0)
    + (lists:sum(FRates) * MinutesLeftAfterOpeningValves).

calc_max_extra_flow_potential([FRate | Next], AccParcels) ->
    AccParcels + calc_max_extra_flow_potential(Next, AccParcels + (FRate * 2));
calc_max_extra_flow_potential([], AccParcels) ->
    AccParcels.

open_decision(NextCandidate, Search) ->
    UpdatedCandidate = NextCandidate#valve{open = true},
    UpdatedAll = maps:update(UpdatedCandidate#valve.id, UpdatedCandidate, Search#search.all),
    NewTotalFlow = Search#search.total_flow + (Search#search.flow_parcels * 2),
    Search#search{
        total_flow = NewTotalFlow,
        flow_parcels = Search#search.flow_parcels + NextCandidate#valve.frate,
        clock = Search#search.clock + 2, % one minute to get there, another to open
        current = UpdatedCandidate#valve.id,
        all = UpdatedAll,
        rev_path = [{open, NextCandidate#valve.id, {new_flow, NewTotalFlow}}
                    | Search#search.rev_path],
        nr_of_valves_opened = Search#search.nr_of_valves_opened + 1
    }.

walk_by_decision(NextCandidate, Search) ->
    NewTotalFlow = Search#search.total_flow + Search#search.flow_parcels,
    Search#search{
        total_flow = Search#search.total_flow + Search#search.flow_parcels,
        clock = Search#search.clock + 1,
        current = NextCandidate#valve.id,
        rev_path = [{walk_by, NextCandidate#valve.id, {new_flow, NewTotalFlow}}
                    | Search#search.rev_path]
    }.

log(Format, Args) ->
    io:format(standard_error, Format ++ "\n", Args).
