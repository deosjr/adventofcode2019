:- use_module(library(clpfd)).

input(Input) :-
    read_file_to_string("day6.input", String, []),
    split_string(String, "\n", "", SplitRaw),
    select("", SplitRaw, Split),
    maplist([In, From-To]>>split_string(In, ")", "", [From, To]), Split, Input).

relations(Orbits, Relations) :-
    findall(X, member(_-X, Orbits), NonRoots),
    Names = ["COM"|NonRoots],
    maplist({Orbits}/[In, In-Children]>>children(Orbits, In, Children), Names, Relations).

children(Orbits, Name, Children) :-
    findall(Child, member(Name-Child, Orbits), Children).

tree(Relations, Current, node(Current, [])) :-
    \+(memberchk(Current-_, Relations) ).
tree(Relations, Current, Tree) :-
    selectchk(Current-Children, Relations, RemRelations),
    maplist({RemRelations}/[Child, SubTree]>>tree(RemRelations, Child, SubTree), Children, ChildTrees),
    Tree = node(Current, ChildTrees).

part1(node(_, []), X, X).
part1(node(_, Children), Depth, Ans) :-
    Children \= [],
    N #= Depth + 1,
    maplist({N}/[In, Out]>>part1(In, N, Out), Children, Depths),
    sum(Depths, #=, SumDepths),
    Ans #= SumDepths + Depth.

transfers(Tree, From, To, Ans) :-
    path(Tree, From, FromPath),
    path(Tree, To, ToPath),
    length(FromPath, X),
    length(ToPath, Y),
    longest_common_prefix(FromPath, ToPath, Prefix),
    length(Prefix, Z),
    % we substract the common node twice
    % but we also count the last step to both From/To
    Ans #= X + Y - 2*Z.

path(node(Name, Children), End, Path) :-
    ( memberchk(node(End,_), Children)
    ->
        Path = [End]
    ;
        member(Child, Children),
        path(Child, End, RPath),
        Path = [Name|RPath]
    ).

longest_common_prefix(A, B, C) :-
    findall(P, (prefix(P,A),prefix(P,B)), Prefixes),
    reverse(Prefixes, [C|_]).

part2(Tree, Ans) :-
    transfers(Tree, "YOU", "SAN", Ans).

run :-
    input(Orbits),
    relations(Orbits, Relations),
    tree(Relations, "COM", Tree),
    part1(Tree, 0, Ans1),
    format('Part 1: ~w~n', [Ans1]),
    part2(Tree, Ans2),
    format('Part 2: ~w~n', [Ans2]).
