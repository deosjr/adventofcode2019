:- use_module(library(clpfd)).

parse(Input) :-
    csv_read_file("day5.input", [Row], []),
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

opcode(3, _, Mem, Input, Index, Ans, NAns) :-
    imm(Mem, Index, 1, X),
    set(X, Mem, Input, NMem), 
    NIndex #= Index+2, 
    run(NMem, Input, NIndex, Ans, NAns).

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

run :-
    parse(Program),
    run(Program, 1, Ans1),
    format('Part 1: ~w~n', [Ans1]),
    run(Program, 5, Ans2),
    format('Part 2: ~w~n', [Ans2]).
