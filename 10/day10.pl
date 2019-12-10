:- use_module(library(clpfd)).

input(Asteroids) :-
    read_file_to_string("day10.input", String, []),
    split_string(String, "\n", "", SplitRaw),
    select("", SplitRaw, Split),
    grid_to_asteroids(Split, Asteroids).
    
grid_to_asteroids(Grid, Asteroids) :-
    maplist([In, Out]>>(string_chars(In, Chars), enumerate(Chars, Out, 0)), Grid, Xs),
    enumerate(Xs, Ys, 0),
    maplist([Y-List, Out]>>convlist({Y}/[A,B]>>(A=X-'#',B=X-Y), List, Out), Ys, XYs),
    flatten(XYs, Asteroids).

enumerate([], [], _).
enumerate([H|T], [I-H|TT], I) :-
    NI #= I + 1,
    enumerate(T, TT, NI).

gcd(X, Y, GCD) :-
    ( X #= 0
    -> 
        GCD #= abs(Y)
    ;
        Z #= Y mod X,
        gcd(Z, X, GCD)
    ).

collision(Asteroids, FX-FY, TX-TY, DX-DY) :-
    CX #= FX+DX,
    CY #= FY+DY,
    CX-CY \= TX-TY,
    (
        memberchk(CX-CY, Asteroids)
    ;
        collision(Asteroids, CX-CY, TX-TY, DX-DY)
    ).

detect(From, Asteroids, Detected) :-
    detect(From, 0, Asteroids, Detected).

detect(_, Index, Asteroids, 0) :-
    length(Asteroids, Index).

detect(FX-FY, Index, Asteroids, Detected) :-
    length(Asteroids, N),
    Index #< N,
    nth0(Index, Asteroids, TX-TY),
    NI #= Index + 1,
    detect(FX-FY, NI, Asteroids, PrevDetected),
    DiffX #= TX-FX,
    DiffY #= TY-FY,
    gcd(DiffX, DiffY, Step),
    DX #= DiffX // Step,
    DY #= DiffY // Step,
    ( collision(Asteroids, FX-FY, TX-TY, DX-DY)
    ->
        Detected #= PrevDetected
    ;
        Detected #= PrevDetected+1
    ).

part1(Asteroids, Ans, StationX, StationY) :-
    maplist({Asteroids}/[A, D-A]>>(selectchk(A, Asteroids, Rem), detect(A, Rem, D)), Asteroids, List),
    max_member(Ans-(StationX-StationY), List).

part2(Asteroids, SX, SY, Out) :-
    % maplist Asteroids to Order-Asteroid
    % collect Asteroids with same order in one list under key
    % sort the list by Order
    % cycle through the list and consume asteroids
    % return the 200th asteroid consumed
    % TODO:
    Out = 42.

run :-
    input(Asteroids),
    part1(Asteroids, Detected, StationX, StationY),
    format('Part 1: ~w~n', [Detected]),
    part2(Asteroids, StationX, StationY, Ans),
    format('Part 2: ~w~n', [Ans]).

:- begin_tests(part1).

test(simple_grid) :-
    Grid = [".#..#",
            ".....",
            "#####",
            "....#",
            "...##"],
    grid_to_asteroids(Grid, Asteroids),
    part1(Asteroids, D, SX, SY),
    assertion( 8 == D ),
    assertion( SX-SY == 3-4 ).

:- end_tests(part1).
