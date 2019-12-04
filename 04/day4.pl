:- use_module(library(clpfd)).

password_base(P, Pass) :-
    P = [A,B,C,D,E,F],
    P ins 0..9,
    chain(P, #=<),
    labeling([ff],P),
    Pass #= 100000 * A + 10000 * B + 1000 * C + 100 * D + 10 * E + F,
    \+(all_distinct(P)).

password1(Pass) :-
    password_base(_, Pass).

password2(Pass) :-
    password_base(P, Pass),
    Cardinalities = [1-_,2-_,3-_,4-_,5-_,6-_,7-_,8-_,9-_],
    global_cardinality(P, Cardinalities),
    selectchk(_-2, Cardinalities, _).

loop(Min, Max, PasswordFunc, N) :-
    P in Min..Max,
    bagof(P, call(PasswordFunc, P), Passwords),
    length(Passwords, N).

run :-
    loop(307237, 769058, password1, Out1),
    format("Part 1: ~w~n", Out1),
    loop(307237, 769058, password2, Out2),
    format("Part 2: ~w~n", Out2).
