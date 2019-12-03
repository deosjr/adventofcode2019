:- use_module(library(clpfd)).

input([Wire1, Wire2]) :-
    csv_read_file("day3.input", [Row1, Row2], []),
    Row1 =.. [_|RWire1],
    maplist(parse_instruction, RWire1, Wire1),
    Row2 =.. [_|RWire2],
    maplist(parse_instruction, RWire2, Wire2).

parse_instruction(Instr, [Direction, N]) :-
    string_chars(Instr, [Direction|NRaw]),
    string_chars(NS, NRaw),
    number_string(N, NS).

wire_to_segments(Wire, Segments) :-
    segment_loop(Wire, Segments, 0, 0, 0).

segment_loop([], [], _, _, _).
segment_loop([Instr|T], [S|TSegments], X, Y, Distance) :-
    Instr = [D, N],
    (   D='U', NX#=X, NY#=Y+N,
        S = segment{fixed_axis:x, distance_start:Distance, fixed:X, begin:Y, end:NY}
    ;   D='R', NX#=X+N, NY#=Y,
        S = segment{fixed_axis:y, distance_start:Distance, fixed:Y, begin:X, end:NX}
    ;   D='D', NX#=X, NY#=Y-N,
        S = segment{fixed_axis:x, distance_start:Distance, fixed:X, begin:Y, end:NY}
    ;   D='L', NX#=X-N, NY#=Y,
        S = segment{fixed_axis:y, distance_start:Distance, fixed:Y, begin:X, end:NX}
    ),
    NDistance #= Distance + N,
    segment_loop(T, TSegments, NX, NY, NDistance). 

find_min_collision(D1, D2, CollisionFunc, Out) :-
    findall(X,
    (
    select(S1, D1, _),
    select(S2, D2, _),
    call(CollisionFunc, S1, S2, X)
    ),
    Delay),
    foldl([A,B,C]>>(C#=min(A,B)), Delay, 99999, Out).
    

collision(W, V, M) :-
    W.fixed_axis \= V.fixed_axis,
    ((W.begin #< V.fixed, W.end #> V.fixed) ; (W.end #< V.fixed, W.begin #> V.fixed)),
    ((V.begin #< W.fixed, V.end #> W.fixed) ; (V.end #< W.fixed, V.begin #> W.fixed)),
    M #= abs(W.fixed) + abs(V.fixed).

collision2(W, V, M) :-
    W.fixed_axis \= V.fixed_axis,
    ((W.begin #< V.fixed, W.end #> V.fixed) ; (W.end #< V.fixed, W.begin #> V.fixed)),
    ((V.begin #< W.fixed, V.end #> W.fixed) ; (V.end #< W.fixed, V.begin #> W.fixed)),
    M #= W.distance_start + abs(V.fixed - W.begin) + V.distance_start + abs(W.fixed - V.begin).

part1(D1, D2, Out) :-
    find_min_collision(D1, D2, collision, Out).

part2(D1, D2, Out) :-
    find_min_collision(D1, D2, collision2, Out).

run :-
    input([Wire1, Wire2]),
    wire_to_segments(Wire1, Segments1),
    wire_to_segments(Wire2, Segments2),
    part1(Segments1, Segments2, Ans1),
    format('Part 1: ~w~n', [Ans1]),
    part2(Segments1, Segments2, Ans2),
    format('Part 2: ~w~n', [Ans2]).
