:- use_module(library(clpfd)).

input(Input) :-
    read_file_to_string("day8.input", String, []),
    string_chars(String, Chars),
    select('\n', Chars, CharsNoNewline),
    maplist([In, Out]>>number_chars(Out, [In]), CharsNoNewline, Input).

layers([], [], _).
layers(Input, [Layer|Layers], Size) :-
    length(Layer, Size),
    append(Layer, Rem, Input),
    layers(Rem, Layers, Size).

zeroes_ones_twos(Layer, Zeroes, Ones, Twos) :-
    Cardinalities = [0-Zeroes,1-Ones,2-Twos],
    global_cardinality(Layer, Cardinalities).

part1(Layers, Ans) :-
    maplist([In, Z-O-T]>>zeroes_ones_twos(In, Z, O, T), Layers, Counts),
    keysort(Counts, [Z-O-T|_]),
    Ans #= O * T.

apply_layer([], [], []).
apply_layer([HLayer|TLayer], [HImage|TImage], Out) :-
    apply_layer(TLayer, TImage, Temp),
    ( (var(HImage), HLayer\=2)
    -> 
        Out = [HLayer|Temp]
    ;
        Out = [HImage|Temp]
    ).

part2(Layers) :-
    length(Image, 150),
    foldl(apply_layer, Layers, Image, Out),
    maplist([In, Out]>>(In=1->Out='x';Out=' '), Out, Ascii),
    layers(Ascii, Img, 25),
    maplist([In]>>(string_chars(Str, In), writeln(Str)), Img).

run :-
    input(Input),
    layers(Input, Layers, 150), % = 25 * 6
    part1(Layers, Out1),
    format("Part 1: ~w~n", Out1),
    writeln("Part 2:"),
    part2(Layers).
