:- use_module(library(clpfd)).
:- dynamic reaction/2.

input(Input) :-
    read_file_to_string("day14.input", String, []),
    split_string(String, "\n", "\n", Input).

parse(Lines, Reactions) :-
    maplist([In, Out]>>parse_line(In, Out), Lines, Reactions).

parse_chem(Raw, chem(Unit, Chemical)) :-
    split_string(Raw, " ", " ", [U, Chemical]),
    number_string(Unit, U).

parse_line(Line, reaction(ChemOut, ChemsIn)) :-
    split_string(Line, "=>", " ", [Input, "", Output]),
    split_string(Input, ",", " ", Inputs),
    parse_chem(Output, ChemOut),
    maplist(parse_chem, Inputs, ChemsIn).

find_ore_needed([], _, 0).

find_ore_needed([X-"ORE"|ToFind], LeftOver, NumOre) :-
    find_ore_needed(ToFind, LeftOver, N),
    NumOre #= N + X.

find_ore_needed([X-Chem|ToFind], LeftOver, NumOre) :-
    find_in_leftovers(Chem, X, Rem, LeftOver, RemLeftOver),
    (
        Rem #= 0,
        find_ore_needed(ToFind, RemLeftOver, NumOre)
    ;
        Rem #\= 0,
        reaction(chem(Y, Chem), Inputs),
        multiplier(Rem, Y, M, Left),
        maplist({M}/[chem(Z, C),Out]>>(N#=Z*M,Out=N-C), Inputs, InputsToFind),
        append(ToFind, InputsToFind, NewToFind),
        (
            Left #= 0,
            NewLeftOver = RemLeftOver
        ;
            Left #> 0,
            NewLeftOver = [Left-Chem|RemLeftOver]
        ),
        find_ore_needed(NewToFind, NewLeftOver, NumOre)
    ).

multiplier(X, Y, 1, Left) :-
    X #=< Y,
    Left #= Y - X.

multiplier(X, Y, M, Left) :-
    X #> Y,
    FloorM #= X // Y,
    Rem #= X rem Y,
    (
        Rem #= 0,
        M #= FloorM
    ;
        Rem #> 0,
        M #= FloorM + 1
    ),
    Left #= Y*M - X.

find_in_leftovers(Chem, X, Rem, LeftIn, LeftOut) :-
    ( selectchk(N-Chem, LeftIn, RemLeft)
    ->
        (
            N #= X,
            Rem #= 0,
            LeftOut = RemLeft
        ;
            N #< X,
            Rem #= X - N,
            LeftOut = RemLeft
        ;
            N #> X,
            Rem #= 0,
            Left #= N - X,
            LeftOut = [Left-Chem|RemLeft]
        )
    ;
        Rem #= X,
        LeftOut = LeftIn
    ).

part1(Input, Ans) :-
    parse(Input, Reactions),
    maplist(assertz, Reactions),
    find_ore_needed([1-"FUEL"], [], Ans),
    retractall(reaction(_, _)).

run :-
    input(Input),
    part1(Input, Ans1),
    format("Part 1: ~w~n", [Ans1]).

:- begin_tests(multiplier).

test(lessthan) :-
    multiplier(3, 5, 1, 2).

test(equals) :-
    multiplier(20, 20, 1, 0).

test(greaterthan) :-
    multiplier(7, 2, 4, 1).

:- end_tests(multiplier).

:- begin_tests(leftovers).

test(find_in_leftovers) :-
    find_in_leftovers("A", 4, Rem, [3-"A"], Left),
    assertion(Rem == 1),
    assertion(Left == []).
    
test(find_all_in_leftovers) :-
    find_in_leftovers("A", 2, Rem, [5-"A"], Left),
    assertion(Rem == 0),
    assertion(Left == [3-"A"]).

test(dont_find_in_leftovers) :-
    find_in_leftovers("B", 4, Rem, [3-"A"], Left),
    assertion(Rem == 4),
    assertion(Left == [3-"A"]).

:- end_tests(leftovers).

:- begin_tests(part1).

test(small_example) :-
    Input = ["10 ORE => 10 A",
             "1 ORE => 1 B", 
             "7 A, 1 B => 1 C", 
             "7 A, 1 C => 1 D", 
             "7 A, 1 D => 1 E", 
             "7 A, 1 E => 1 FUEL"],
    part1(Input, Ans),
    assertion(Ans == 31).

test(second_example) :-
    Input = ["9 ORE => 2 A",
             "8 ORE => 3 B", 
             "7 ORE => 5 C", 
             "3 A, 4 B => 1 AB",
             "5 B, 7 C => 1 BC",
             "4 C, 1 A => 1 CA",
             "2 AB, 3 BC, 4 CA => 1 FUEL"],
    part1(Input, Ans),
    assertion(Ans == 165).

test(larger_example1) :-
    Input = ["157 ORE => 5 NZVS",
             "165 ORE => 6 DCFZ",
             "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
             "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
             "179 ORE => 7 PSHF",
             "177 ORE => 5 HKGWZ",
             "7 DCFZ, 7 PSHF => 2 XJWVT",
             "165 ORE => 2 GPVTF",
             "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"],
    part1(Input, Ans),
    assertion(Ans == 13312).

:- end_tests(part1).
