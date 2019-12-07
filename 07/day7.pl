:- use_module(library(clpfd)).

parse(Input) :-
    csv_read_file("day7.input", [Row], []),
    Row =.. [_|Input].

set(I, L, N, X) :-
    nth0(I, L, _, R),
    nth0(I, X, N, R).

% pos is a positional mode read from memory
% imm is an immediate mode read
% mod means either one depending on first value
% NOTE: set instruction can only ever be positional
% but we read the ip value as imm first
imm(Mem, Index, Pos, Value) :-
    I #= Index + Pos,
    nth0(I, Mem, Value).

pos(Mem, Index, Pos, Value) :-
    imm(Mem, Index, Pos, X),
    imm(Mem, X, 0, Value).

pos_or_imm(Mem, First, Index, Pos, Value) :-
    Rem #= First // (10 * (10^Pos)),
    ( Rem mod 10 #= 0
    ->
        pos(Mem, Index, Pos, Value)
    ;
        imm(Mem, Index, Pos, Value)
    ).

mod_mod(Mem, First, Index, X, Y) :-
    pos_or_imm(Mem, First, Index, 1, X),
    pos_or_imm(Mem, First, Index, 2, Y).

mod_mod_imm(Mem, First, Index, X, Y, Z) :-
    pos_or_imm(Mem, First, Index, 1, X),
    pos_or_imm(Mem, First, Index, 2, Y),
    imm(Mem, Index, 3, Z).

opcode(1, First, Mem, Input, Index, Ans, NAns) :-
    mod_mod_imm(Mem, First, Index, X, Y, Z),
    New #= X+Y, 
    set(Z, Mem, New, NMem), 
    NIndex #= Index+4, 
    run(NMem, Input, NIndex, Ans, NAns).

opcode(2, First, Mem, Input, Index, Ans, NAns) :-
    mod_mod_imm(Mem, First, Index, X, Y, Z),
    New #= X*Y, 
    set(Z, Mem, New, NMem), 
    NIndex #= Index+4, 
    run(NMem, Input, NIndex, Ans, NAns).

opcode(3, _, Mem, [Input|RInput], Index, Ans, NAns) :-
    imm(Mem, Index, 1, X),
    set(X, Mem, Input, NMem), 
    NIndex #= Index+2, 
    run(NMem, RInput, NIndex, Ans, NAns).

opcode(4, First, Mem, Input, Index, _, NAns) :-
    pos_or_imm(Mem, First, Index, 1, Output),
    NIndex #= Index+2, 
    run(Mem, Input, NIndex, Output, NAns).

opcode(5, First, Mem, Input, Index, Ans, NAns) :-
    mod_mod(Mem, First, Index, X, Y),
    ( X #\= 0
    ->
        run(Mem, Input, Y, Ans, NAns)
    ;
        NIndex #= Index+3,
        run(Mem, Input, NIndex, Ans, NAns)
    ).

opcode(6, First, Mem, Input, Index, Ans, NAns) :-
    mod_mod(Mem, First, Index, X, Y),
    ( X #= 0
    ->
        run(Mem, Input, Y, Ans, NAns)
    ;
        NIndex #= Index+3,
        run(Mem, Input, NIndex, Ans, NAns)
    ).

opcode(7, First, Mem, Input, Index, Ans, NAns) :-
    mod_mod_imm(Mem, First, Index, X, Y, Z),
    ( X #< Y
    ->
        set(Z, Mem, 1, NMem)
    ;
        set(Z, Mem, 0, NMem)
    ),
    NIndex #= Index+4, 
    run(NMem, Input, NIndex, Ans, NAns).

opcode(8, First, Mem, Input, Index, Ans, NAns) :-
    mod_mod_imm(Mem, First, Index, X, Y, Z),
    ( X #= Y
    ->
        set(Z, Mem, 1, NMem)
    ;
        set(Z, Mem, 0, NMem)
    ),
    NIndex #= Index+4, 
    run(NMem, Input, NIndex, Ans, NAns).

opcode(99, _, _, _, _, X, X).

run(Mem, Input, Index, Ans, NAns) :-
    nth0(Index, Mem, First),
    Opcode #= First mod 100,
    opcode(Opcode, First, Mem, Input, Index, Ans, NAns).

run(Program, Input, Ans) :-
    run(Program, Input, 0, -1, Ans).

phases(Phases, Min, Max) :-
    length(Phases, 5),
    Phases ins Min..Max,
    all_distinct(Phases).

run_amps(_, [], X, X).
run_amps(Program, [Phase|RPhases], Input, Output) :-
    run(Program, [Phase, Input], Temp),
    run_amps(Program, RPhases, Temp, Output).

part1(Program, Out) :-
    phases(Phases, 0, 4),
    findall(Ans, run_amps(Program, Phases, 0, Ans), Answers),
    max_list(Answers, Out).

% TODO
part2(Program, Out) :-
    Out=42.
    %phases(Phases, 5, 9),

run :-
    parse(Program),
    part1(Program, Ans1),
    format('Part 1: ~w~n', [Ans1]),
    part2(Program, Ans2),
    format('Part 2: ~w~n', [Ans2]).
