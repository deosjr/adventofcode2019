:- use_module(library(clpfd)).
:- dynamic mem/2.

run :-
    parse(Program),
    run_to_halt(Program, [1], Ans1),
    format('Part 1: ~w~n', Ans1),
    run_to_halt(Program, [2], Ans2),
    format('Part 2: ~w~n', Ans2).

parse(Input) :-
    csv_read_file("day9.input", [Row], []),
    Row =.. [_|Input].

enumerate([], [], _).
enumerate([H|T], [I-H|TT], I) :-
    NI #= I + 1,
    enumerate(T, TT, NI).

list_to_mem(List) :-
    enumerate(List, OrdPairs, 0),
    maplist([I-X]>>assertz(mem(I, X)), OrdPairs).

set(Index, Value) :-
    retractall(mem(Index,_)),
    assertz(mem(Index, Value)).

get(Index, Value) :-
    ( mem(Index, X) 
    ->
        Value #= X
    ;
        Value #= 0
    ).

mode(0, Index, Pos, _, Value) :-
    mode(1, Index, Pos, _, X),
    get(X, Value).

mode(1, Index, Pos, _, Value) :-
    I #= Index + Pos,
    get(I, Value).

mode(2, Index, Pos, RelativeBase, Value) :-
    mode(1, Index, Pos, _, X),
    Rel #= X + RelativeBase,
    get(Rel, Value).

value(First, Index, Pos, RelBase, Value) :-
    Mod #= (First // (10 * (10^Pos))) mod 10,
    Mod in 0..2,
    mode(Mod, Index, Pos, RelBase, Value).

addr(First, Index, Pos, RelBase, Value) :-
    Mod #= (First // (10 * (10^Pos))) mod 10,
    I #= Index + Pos,
    ( Mod #= 0 
    ->
        get(I, Value)
    ;
        get(I, X),
        Value #= X + RelBase
    ).

value_value(First, Index, RelBase, X, Y) :-
    value(First, Index, 1, RelBase, X),
    value(First, Index, 2, RelBase, Y).

value_value_addr(First, Index, RelBase, X, Y, Z) :-
    value(First, Index, 1, RelBase, X),
    value(First, Index, 2, RelBase, Y),
    addr(First, Index, 3, RelBase, Z).

opcode(1, First, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value_addr(First, Index, RB, X, Y, Z),
    New #= X+Y, 
    set(Z, New), 
    NewIndex #= Index+4. 

opcode(2, First, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value_addr(First, Index, RB, X, Y, Z),
    New #= X*Y, 
    set(Z, New), 
    NewIndex #= Index+4. 

opcode(3, First, [Input|RemInput], RemInput, Index, NewIndex, RB, RB, _, false) :-
    addr(First, Index, 1, RB, X),
    set(X, Input), 
    NewIndex #= Index+2. 

opcode(4, First, I, I, Index, NewIndex, RB, RB, Output, false) :-
    value(First, Index, 1, RB, Output),
    NewIndex #= Index+2. 

opcode(5, First, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value(First, Index, RB, X, Y),
    ( X #\= 0
    ->
        NewIndex #= Y
    ;
        NewIndex #= Index+3
    ).

opcode(6, First, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value(First, Index, RB, X, Y),
    ( X #= 0
    ->
        NewIndex #= Y
    ;
        NewIndex #= Index+3
    ).

opcode(7, First, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value_addr(First, Index, RB, X, Y, Z),
    ( X #< Y
    ->
        set(Z, 1)
    ;
        set(Z, 0)
    ),
    NewIndex #= Index+4. 

opcode(8, First, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value_addr(First, Index, RB, X, Y, Z),
    ( X #= Y
    ->
        set(Z, 1)
    ;
        set(Z, 0)
    ),
    NewIndex #= Index+4. 

opcode(9, First, I, I, Index, NewIndex, RelBase, NewRelBase, _, false) :-
    value(First, Index, 1, RelBase, X),
    NewRelBase #= RelBase + X,
    NewIndex #= Index+2.

opcode(99, _, _, _, _, _, _, _, 0, true).

run(Input, RemInput, Index, NewIndex, RelBase, NewRelBase, Output, ProgramHalted) :-
    get(Index, First),
    Opcode #= First mod 100,
    opcode(Opcode, First, Input, NInput, Index, NIndex, RelBase, NRelBase, Out, Halted),
    ( not(var(Out))
    ->
        ( Halted=true
        ->
            ProgramHalted=true
        ;
            ProgramHalted=false,
            Output=Out 
        ),
        RemInput=NInput,
        NewIndex=NIndex,
        NewRelBase=NRelBase
    ;
        run(NInput, RemInput, NIndex, NewIndex, NRelBase, NewRelBase, Output, ProgramHalted)
    ).

run(Input, Output) :-
    run(Input, _, 0, _, 0, _, Output, _).

run_to_halt(Program, Input, Output) :-
    list_to_mem(Program),
    run_to_halt(Input, 0, 0, Output),
    retractall(mem(_,_)).

run_to_halt(Input, Index, RelBase, Output) :-
    run(Input, NewInput, Index, NewIndex, RelBase, NewRelBase, Out, Halted),
    ( Halted = true
    ->
        Output = []
    ;
        run_to_halt(NewInput, NewIndex, NewRelBase, NOut),
        Output = [Out|NOut]
    ).

:- begin_tests(part1).

test(quine) :-
    Program = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99],
    run_to_halt(Program, [], Output),
    assertion(Output = Program).

test(large_multiplication) :-
    Program = [1102,34915192,34915192,7,4,7,99,0],
    run_to_halt(Program, [], Ans),
    assertion(Ans == [1219070632396864]).

test(large_number) :-
    Program = [104,1125899906842624,99],
    run_to_halt(Program, [], Ans),
    assertion(Ans == [1125899906842624]).

:- end_tests(part1).
