:- use_module(library(clpfd)).

input(Input) :-
    read_file_to_string("day1.input", String, []),
    split_string(String, "\n", "", SplitRaw),
    select("", SplitRaw, Split),
    maplist([In, Out]>>number_string(Out,In), Split, Input).

fuel(X, Y) :-
    Y #= max(0, (X//3)-2).

part1(Input, Ans) :-
    foldl([A,B,C]>>(fuel(A,D),C#=B+D), Input, 0, Ans).

iterative_fuel(X, 0) :-
    X #=< 0.
iterative_fuel(X, Y) :-
    X #> 0,
    fuel(X, Z),
    iterative_fuel(Z, RZ),
    Y #= Z + RZ.

part2(Input, Ans) :-
    foldl([A,B,C]>>(iterative_fuel(A,D),C#=B+D), Input, 0, Ans).

run :-
    input(Input),
    part1(Input, Ans1),
    format('Part 1: ~w~n', [Ans1]),
    part2(Input, Ans2),
    format('Part 2: ~w~n', [Ans2]).
