:- use_module(library(clpfd)).

run :-
    parse(Program),
    run(Program, [1], Ans1),
    format('Part 1: ~w~n', [Ans1]),
    run(Program, [2], Ans2),
    format('Part 2: ~w~n', [Ans2]).

parse(Input) :-
    csv_read_file("day9.input", [Row], []),
    Row =.. [_|Input].

set(Index, Mem, Value, NewMem) :-
    length(Mem, N),
    ( Index #< N
    ->
        replace2(Index, Mem, Value, NewMem)
    ;
        NewLen #= Index+1,
        pad_zeroes(Mem, NMem, NewLen),
        replace2(Index, NMem, Value, NewMem)
    ).

% Slow: 1m3s ; profiler says nth0 ~77%
replace(Index, Mem, Value, NewMem) :-
    nth0(Index, Mem, _, R),
    nth0(Index, NewMem, Value, R).

% Better: 47s (plucked from the internet)
% profiler says =.. ~77%
replace2(Index, Mem, Value, NewMem) :-
    Dummy =.. [dummy|Mem],
    J #= Index + 1,
%    somehow this gives uninstantiated error on index in test?
%    format("~w ~w ~w ~w~n", [Index, J, Dummy, Value]),
    setarg(J, Dummy, Value),
    Dummy =.. [dummy|NewMem].

get(Index, Mem, Value) :-
    length(Mem, N),
    ( Index #< N 
    ->
        nth0(Index, Mem, Value)
    ;
        Value = 0
    ).

pad_zeroes(Mem, NewMem, NewLength) :-
    length(NewMem, NewLength),
    append(Mem, Zeroes, NewMem),
    Zeroes ins 0..0.

mode(0, Mem, Index, Pos, _, Value) :-
    mode(1, Mem, Index, Pos, _, X),
    get(X, Mem, Value).

mode(1, Mem, Index, Pos, _, Value) :-
    I #= Index + Pos,
    get(I, Mem, Value).

mode(2, Mem, Index, Pos, RelativeBase, Value) :-
    mode(1, Mem, Index, Pos, _, X),
    Rel #= X + RelativeBase,
    get(Rel, Mem, Value).

value(Mem, First, Index, Pos, RelBase, Value) :-
    Mod #= (First // (10 * (10^Pos))) mod 10,
    Mod in 0..2,
    mode(Mod, Mem, Index, Pos, RelBase, Value).

addr(Mem, First, Index, Pos, RelBase, Value) :-
    Mod #= (First // (10 * (10^Pos))) mod 10,
    I #= Index + Pos,
    ( Mod #= 0 
    ->
        get(I, Mem, Value)
    ;
        get(I, Mem, X),
        Value #= X + RelBase
    ).

value_value(Mem, First, Index, RelBase, X, Y) :-
    value(Mem, First, Index, 1, RelBase, X),
    value(Mem, First, Index, 2, RelBase, Y).

value_value_addr(Mem, First, Index, RelBase, X, Y, Z) :-
    value(Mem, First, Index, 1, RelBase, X),
    value(Mem, First, Index, 2, RelBase, Y),
    addr(Mem, First, Index, 3, RelBase, Z).

opcode(1, First, Mem, NewMem, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value_addr(Mem, First, Index, RB, X, Y, Z),
    New #= X+Y, 
    set(Z, Mem, New, NewMem), 
    NewIndex #= Index+4. 

opcode(2, First, Mem, NewMem, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value_addr(Mem, First, Index, RB, X, Y, Z),
    New #= X*Y, 
    set(Z, Mem, New, NewMem), 
    NewIndex #= Index+4. 

opcode(3, First, Mem, NewMem, [Input|RemInput], RemInput, Index, NewIndex, RB, RB, _, false) :-
    addr(Mem, First, Index, 1, RB, X),
    set(X, Mem, Input, NewMem), 
    NewIndex #= Index+2. 

opcode(4, First, Mem, Mem, I, I, Index, NewIndex, RB, RB, Output, false) :-
    value(Mem, First, Index, 1, RB, Output),
    NewIndex #= Index+2. 

opcode(5, First, Mem, Mem, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value(Mem, First, Index, RB, X, Y),
    ( X #\= 0
    ->
        NewIndex #= Y
    ;
        NewIndex #= Index+3
    ).

opcode(6, First, Mem, Mem, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value(Mem, First, Index, RB, X, Y),
    ( X #= 0
    ->
        NewIndex #= Y
    ;
        NewIndex #= Index+3
    ).

opcode(7, First, Mem, NewMem, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value_addr(Mem, First, Index, RB, X, Y, Z),
    ( X #< Y
    ->
        set(Z, Mem, 1, NewMem)
    ;
        set(Z, Mem, 0, NewMem)
    ),
    NewIndex #= Index+4. 

opcode(8, First, Mem, NewMem, I, I, Index, NewIndex, RB, RB, _, false) :-
    value_value_addr(Mem, First, Index, RB, X, Y, Z),
    ( X #= Y
    ->
        set(Z, Mem, 1, NewMem)
    ;
        set(Z, Mem, 0, NewMem)
    ),
    NewIndex #= Index+4. 

opcode(9, First, Mem, Mem, I, I, Index, NewIndex, RelBase, NewRelBase, _, false) :-
    value(Mem, First, Index, 1, RelBase, X),
    NewRelBase #= RelBase + X,
    NewIndex #= Index+2.

opcode(99, _, _, _, _, _, _, _, _, _, 0, true).

run(Mem, NewMem, Input, RemInput, Index, NewIndex, RelBase, NewRelBase, Output, ProgramHalted) :-
    get(Index, Mem, First),
    Opcode #= First mod 100,
    opcode(Opcode, First, Mem, NMem, Input, NInput, Index, NIndex, RelBase, NRelBase, Out, Halted),
    ( not(var(Out))
    ->
        ( Halted=true
        ->
            ProgramHalted=true
        ;
            ProgramHalted=false,
            Output=Out 
        ),
        NewMem=NMem,
        RemInput=NInput,
        NewIndex=NIndex,
        NewRelBase=NRelBase
    ;
        run(NMem, NewMem, NInput, RemInput, NIndex, NewIndex, NRelBase, NewRelBase, Output, ProgramHalted)
    ).

run(Program, Input, Output) :-
    run(Program, _, Input, _, 0, _, 0, _, Output, _).

run_to_halt(Program, Input, Output) :-
    run_to_halt(Program, Input, 0, 0, Output).

run_to_halt(Program, Input, Index, RelBase, Output) :-
    run(Program, NewProgram, Input, NewInput, Index, NewIndex, RelBase, NewRelBase, Out, Halted),
    ( Halted = true
    ->
        Output = []
    ;
        run_to_halt(NewProgram, NewInput, NewIndex, NewRelBase, NOut),
        Output = [Out|NOut]
    ).

:- begin_tests(opcodes).

test(add) :-
    Mem = [1101, 3, 5, 4, 0],
    opcode(1, 1101, Mem, NewMem, _, _, 0, NewIndex, _, _, _, Halted),
    assertion(NewMem == [1101, 3, 5, 4, 8]),
    assertion(NewIndex == 4),
    assertion(Halted == false).

test(multiply) :-
    Mem = [1102, 3, 5, 4, 0],
    opcode(2, 1102, Mem, NewMem, _, _, 0, NewIndex, _, _, _, Halted),
    assertion(NewMem == [1102, 3, 5, 4, 15]),
    assertion(NewIndex == 4),
    assertion(Halted == false).

test(input) :-
    Mem = [103, 3],
    Input = [2, 3, 1],
    opcode(3, 103, Mem, NewMem, Input, RemInput, 0, NewIndex, _, _, _, Halted),
    assertion(NewMem == [2, 3]),
    assertion(RemInput == [3, 1]),
    assertion(NewIndex == 2),
    assertion(Halted == false).

test(output) :-
    Mem = [104, 3],
    opcode(4, 104, Mem, NewMem, _, _, 0, NewIndex, _, _, Output, Halted),
    assertion(NewMem == Mem),
    assertion(NewIndex == 2),
    assertion(Output == 3),
    assertion(Halted == false).

test(output_pos) :-
    Mem = [4, 3],
    opcode(4, 4, Mem, NewMem, _, _, 0, NewIndex, _, _, Output, Halted),
    assertion(NewMem == Mem),
    assertion(NewIndex == 2),
    assertion(Output == 0),
    assertion(Halted == false).

test(jump_if_not_zero) :-
    Mem = [5, 3, 5],
    opcode(5, 5, Mem, NewMem, _, _, 0, NewIndex, _, _, _, Halted),
    assertion(NewMem == Mem),
    assertion(NewIndex == 3),
    assertion(Halted == false).

test(jump_if_zero) :-
    Mem = [6, 3, 5],
    opcode(6, 5, Mem, NewMem, _, _, 0, NewIndex, _, _, _, Halted),
    assertion(NewMem == Mem),
    assertion(NewIndex == 0),
    assertion(Halted == false).

test(less) :-
    Mem = [1107, 0, 1, 1],
    opcode(7, 1107, Mem, NewMem, _, _, 0, NewIndex, _, _, _, Halted),
    assertion(NewMem == [1107, 1, 1, 1]),
    assertion(NewIndex == 4),
    assertion(Halted == false).

test(equal) :-
    Mem = [1108, 1, 1, 4],
    opcode(8, 1108, Mem, NewMem, _, _, 0, NewIndex, _, _, _, Halted),
    assertion(NewMem == [1108, 1, 1, 4, 1]),
    assertion(NewIndex == 4),
    assertion(Halted == false).

test(relative_base) :-
    Mem = [109, 10],
    RelBase #= 10,
    opcode(9, 109, Mem, NewMem, _, _, 0, NewIndex, RelBase, NewRelBase, _, Halted),
    assertion(NewMem == Mem),
    assertion(NewIndex == 2),
    assertion(NewRelBase == 20),
    assertion(Halted == false).

test(end) :-
    opcode(99, 99, _, _, _, _, 0, _, _, _, _, Halted),
    assertion(Halted == true).

:- end_tests(opcodes).

:- begin_tests(part1).

test(quine) :-
    Program = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99],
    run_to_halt(Program, [], Output),
    assertion(Output == Program).

test(large_multiplication) :-
    Program = [1102,34915192,34915192,7,4,7,99,0],
    run_to_halt(Program, [], Ans),
    assertion(Ans == [1219070632396864]).

test(large_number) :-
    Program = [104,1125899906842624,99],
    run_to_halt(Program, [], Ans),
    assertion(Ans == [1125899906842624]).

:- end_tests(part1).
