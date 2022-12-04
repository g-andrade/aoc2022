#!/usr/bin/env swipl
:- initialization(main, main).

:- use_module(library(pio)).

/*
 * Parsing of input
 */

assignments_input([Number | Next]) --> assignments_pair(Number), assignments_input(Next).
assignments_input([]) --> [].

assignments_pair([RangeA, RangeB]) -->
    range(RangeA), ",", range(RangeB), "\n".

range([Min, Max]) --> nat(Min), "-", nat(Max).

% digit / nat from:
% * https://stackoverflow.com/a/3280788
nat(N)   --> digit(D), nat(D,N).
nat(N,N) --> [].
nat(A,N) --> digit(D), { A1 is A*10 + D }, nat(A1,N).

digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".
digit(6) --> "6".
digit(7) --> "7".
digit(8) --> "8".
digit(9) --> "9".

assignments(Assignments, InputFilename) :-
    phrase_from_file(assignments_input(Assignments), InputFilename).

/*
 * Rules
 */

assignment_contains_another(Ranges, full) :-
    assignment_fully_contains_another(Ranges).
assignment_contains_another(Ranges, partial) :-
    ranges_overlap(Ranges).

assignment_fully_contains_another([RangeA, RangeB]) :-
    range_fully_contains_another(RangeA, RangeB);
    range_fully_contains_another(RangeB, RangeA).

range_fully_contains_another([LeftMin, LeftMax], [RightMin, RightMax]) :-
    LeftMin =< RightMin, LeftMax >= RightMax.

ranges_overlap([RangeA, RangeB]) :-
    range_overlaps(RangeA, RangeB);
    range_overlaps(RangeB, RangeA).

range_overlaps([LeftMin, LeftMax], [RightMin, _RightMax]) :-
    LeftMin =< RightMin, LeftMax >= RightMin.

/*
 * Entry Point
 */

main([InputFilenameAtom, OverlapType]) :-
    atom_codes(InputFilenameAtom, InputFilename),
    assignments(Assignments, InputFilename),
    convlist([X,X]>>assignment_contains_another(X, OverlapType), Assignments,
             OverlappingAssignments),
    length(OverlappingAssignments, Count),
    format('~w~n', [Count]).
