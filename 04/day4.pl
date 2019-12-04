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
    global_cardinality(P, [1-R,2-S,3-T,4-U,5-V,6-W,7-X,8-Y,9-Z]),
    (R#=2;S#=2;T#=2;U#=2;V#=2;W#=2;X#=2;Y#=2;Z#=2).

part2(N) :-
    P in 307237..769058,
    % TODO: why are we finding doubles and I have to filter them out with setof?
    setof(P, password2(P), Passwords),
    length(Passwords, N).

run :-
    part1(Out1),
    format("Part 1: ~w~n", Out1),
    part2(Out2),
    format("Part 2: ~w~n", Out2).
