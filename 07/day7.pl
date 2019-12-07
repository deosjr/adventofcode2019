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

opcode(1, First, Mem, NewMem, Input, RemInput, Index, NewIndex, Output, Halted) :-
    mod_mod_imm(Mem, First, Index, X, Y, Z),
    New #= X+Y, 
    set(Z, Mem, New, NMem), 
    NIndex #= Index+4, 
    run(NMem, NewMem, Input, RemInput, NIndex, NewIndex, Output, Halted).

opcode(2, First, Mem, NewMem, Input, RemInput, Index, NewIndex, Output, Halted) :-
    mod_mod_imm(Mem, First, Index, X, Y, Z),
    New #= X*Y, 
    set(Z, Mem, New, NMem), 
    NIndex #= Index+4, 
    run(NMem, NewMem, Input, RemInput, NIndex, NewIndex, Output, Halted).

opcode(3, _, Mem, NewMem, [Input|RInput], RemInput, Index, NewIndex, Output, Halted) :-
    imm(Mem, Index, 1, X),
    set(X, Mem, Input, NMem), 
    NIndex #= Index+2, 
    run(NMem, NewMem, RInput, RemInput, NIndex, NewIndex, Output, Halted).

opcode(4, First, Mem, Mem, Input, Input, Index, NIndex, Output, false) :-
    pos_or_imm(Mem, First, Index, 1, Output),
    NIndex #= Index+2. 

opcode(5, First, Mem, NewMem, Input, RemInput, Index, NewIndex, Output, Halted) :-
    mod_mod(Mem, First, Index, X, Y),
    ( X #\= 0
    ->
        run(Mem, NewMem, Input, RemInput, Y, NewIndex, Output, Halted)
    ;
        NIndex #= Index+3,
        run(Mem, NewMem, Input, RemInput, NIndex, NewIndex, Output, Halted)
    ).

opcode(6, First, Mem, NewMem, Input, RemInput, Index, NewIndex, Output, Halted) :-
    mod_mod(Mem, First, Index, X, Y),
    ( X #= 0
    ->
        run(Mem, NewMem, Input, RemInput, Y, NewIndex, Output, Halted)
    ;
        NIndex #= Index+3,
        run(Mem, NewMem, Input, RemInput, NIndex, NewIndex, Output, Halted)
    ).

opcode(7, First, Mem, NewMem, Input, RemInput, Index, NewIndex, Output, Halted) :-
    mod_mod_imm(Mem, First, Index, X, Y, Z),
    ( X #< Y
    ->
        set(Z, Mem, 1, NMem)
    ;
        set(Z, Mem, 0, NMem)
    ),
    NIndex #= Index+4, 
    run(NMem, NewMem, Input, RemInput, NIndex, NewIndex, Output, Halted).

opcode(8, First, Mem, NewMem, Input, RemInput, Index, NewIndex, Output, Halted) :-
    mod_mod_imm(Mem, First, Index, X, Y, Z),
    ( X #= Y
    ->
        set(Z, Mem, 1, NMem)
    ;
        set(Z, Mem, 0, NMem)
    ),
    NIndex #= Index+4, 
    run(NMem, NewMem, Input, RemInput, NIndex, NewIndex, Output, Halted).

opcode(99, _, _, _, _, _, _, _, _, true).

run(Mem, NewMem, Input, RemInput, Index, NIndex, Output, Halted) :-
    nth0(Index, Mem, First),
    Opcode #= First mod 100,
    opcode(Opcode, First, Mem, NewMem, Input, RemInput, Index, NIndex, Output, Halted).

phases(Phases, Min, Max) :-
    length(Phases, 5),
    Phases ins Min..Max,
    all_distinct(Phases).

run_amps(_, [], X, X).
run_amps(Program, [Phase|RPhases], Input, Output) :-
    run(Program, _, [Phase, Input], _, 0, _, Temp, _),
    run_amps(Program, RPhases, Temp, Output).

part1(Program, Out) :-
    phases(Phases, 0, 4),
    findall(Ans, run_amps(Program, Phases, 0, Ans), Answers),
    max_list(Answers, Out).

run_amps_with_feedback(Amps, AmpsPtr, LastOutput, Ans) :-
    nth0(AmpsPtr, Amps, Amp),
    Amp = Mem-Index-Input,
    run(Mem, NewMem, Input, NewInput, Index, NewIndex, Output, Halted),
    ( Halted=true
    ->
        Ans = LastOutput
    ;
        NAmpsPtr #= (AmpsPtr+1) mod 5, 
        select(Amp, Amps, NewMem-NewIndex-NewInput, TempAmps),
        nth0(NAmpsPtr, Amps, Amp2),
        Amp2 = Mem2-Index2-Input2,
        append(Input2, [Output], NInput2),
        select(Amp2, TempAmps, Mem2-Index2-NInput2, NewAmps),
        run_amps_with_feedback(NewAmps, NAmpsPtr, Output, Ans)
    ).

part2(Program, Out) :-
    phases([P1, P2, P3, P4, P5], 5, 9),
    Amplifiers = [Program-0-[P1,0],Program-0-[P2],Program-0-[P3],Program-0-[P4],Program-0-[P5]],
    findall(Ans, run_amps_with_feedback(Amplifiers, 0, -1, Ans), Answers),
    max_list(Answers, Out).

run :-
    parse(Program),
    part1(Program, Ans1),
    format('Part 1: ~w~n', [Ans1]),
    part2(Program, Ans2),
    format('Part 2: ~w~n', [Ans2]).
