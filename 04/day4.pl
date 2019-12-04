:- use_module(library(clpfd)).

password(Pass) :-
    P = [A,B,C,D,E,F],
    P ins 0..9,
    chain(P, #=<),
    labeling([ff],P),
    Pass #= 100000 * A + 10000 * B + 1000 * C + 100 * D + 10 * E + F,
    \+(all_distinct(P)).

part1(N) :-
    P in 307237..769058,
    bagof(P, password(P), Passwords),
    length(Passwords, N).

password2(Pass) :-
    P = [A,B,C,D,E,F],
    P ins 0..9,
    chain(P, #=<),
    labeling([ff],P),
    Pass #= 100000 * A + 10000 * B + 1000 * C + 100 * D + 10 * E + F,
    \+(all_distinct(P)),
    Cardinalities = [1-_,2-_,3-_,4-_,5-_,6-_,7-_,8-_,9-_],
    global_cardinality(P, Cardinalities),
    selectchk(_-2, Cardinalities, _).

part2(N) :-
    P in 307237..769058,
    bagof(P, password2(P), Passwords),
    length(Passwords, N).

run :-
    part1(Out1),
    format("Part 1: ~w~n", Out1),
    part2(Out2),
    format("Part 2: ~w~n", Out2).
