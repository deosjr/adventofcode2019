:- use_module(library(clpfd)).

input(Input) :-
    csv_read_file("day2.input", [Row], []),
    Row =.. [_|Input].

set(I, L, N, X) :-
    nth0(I, L, _, R),
    nth0(I, X, N, R).

run(Input, Index, Ans) :-
    length(Input, N),
    Index #< N-3,
    nth0(Index, Input, Opcode),
    I1 #= Index+1, nth0(I1, Input, XI),
    I2 #= Index+2, nth0(I2, Input, YI),
    I3 #= Index+3, nth0(I3, Input, Z),
    nth0(XI, Input, X),
    nth0(YI, Input, Y),
    (
    Opcode=1, New #= X+Y, set(Z,Input,New,NInput), NIndex #= Index+4, run(NInput, NIndex, Ans);
    Opcode=2, New #= X*Y, set(Z,Input,New,NInput), NIndex #= Index+4, run(NInput, NIndex, Ans);
    Opcode=99, nth0(0, Input, Ans)
    ).

run_modified(Input, Noun, Verb, Ans) :-
    set(1, Input, Noun, R),
    set(2, R, Verb, MInput),
    run(MInput, 0, Ans).

part1(Ans) :-
    input(X),
    run_modified(X, 12, 2, Ans).

part2(Ans) :-
    input(X),
    [Noun, Verb] ins 0..99,
    run_modified(X, Noun, Verb, 19690720),
    Ans #= 100 * Noun + Verb.

run :-
    part1(Ans1),
    format('Part 1: ~w~n', [Ans1]),
    part2(Ans2),
    format('Part 2: ~w~n', [Ans2]).
