:- use_module(library(clpfd)).
:- dynamic mem/2.

enumerate([], [], _).
enumerate([H|T], [I-H|TT], I) :-
    NI #= I + 1,
    enumerate(T, TT, NI).

list_to_mem(List) :-
    enumerate(List, OrdPairs, 0),
    maplist([I-X]>>assertz(mem(I, X)), OrdPairs).

clean_mem :-
    retractall(mem(_,_)).

% Config = [ Input, Index, RelBase, Output ]
new_program(Program, Config) :-
    clean_mem,
    list_to_mem(Program),
    Config = [ [], 0, 0, [] ].

give_input(In, [Input, Index, RB, Out], [NewInput, Index, RB, Out]) :-
    append(Input, In, NewInput).

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

opcode(3, First, Input, RemInput, Index, NewIndex, RB, RB, _, false) :-
    (
        Input = []
    ;
        Input = [In|RemInput],
        addr(First, Index, 1, RB, X),
        set(X, In), 
        NewIndex #= Index+2 
    ).

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

run([Input, Index, RelBase, Output], [RemInput, NewIndex, NewRelBase, NewOutput], ProgramHalted) :-
    get(Index, First),
    Opcode #= First mod 100,
    (
        % block on read from empty input
        Opcode = 3,
        Input = [],
        RemInput = [],
        NewIndex = Index,
        NewRelBase = RelBase,
        NewOutput = Output,
        ProgramHalted = false
    ;
        opcode(Opcode, First, Input, NInput, Index, NIndex, RelBase, NRelBase, Out, Halted),
        (
            Halted = true,
            ProgramHalted = true,
            NewOutput = Output
        ;
            Halted = false,
            (
                not(var(Out)),
                append(Output, [Out], NOut)
            ;
                var(Out),
                NOut = Output
            ),
            run([NInput, NIndex, NRelBase, NOut], [RemInput, NewIndex, NewRelBase, NewOutput], ProgramHalted)
        )
    ).

run_to_halt(Program, Input, Output) :-
    new_program(Program, Config),
    give_input(Input, Config, ConfigWithInput),
    run(ConfigWithInput, [_, _, _, Output], true),
    clean_mem.

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
