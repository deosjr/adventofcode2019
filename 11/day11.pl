:- ['intcode.pl'].

parse(Input) :-
    csv_read_file("day11.input", [Row], []),
    Row =.. [_|Input].

turn_left(DX-DY, LX-LY) :-
    LX #= -DY,
    LY #= DX.

turn_right(DX-DY, LX-LY) :-
    LX #= DY,
    LY #= -DX.

left_or_right(0, DX-DY, NX-NY) :-
    turn_left(DX-DY, NX-NY).
left_or_right(1, DX-DY, NX-NY) :-
    turn_right(DX-DY, NX-NY).

camera(X-Y, Panels, Color) :-
    ( memberchk(X-Y-C, Panels)
    ->
        Color = C
    ;
        Color = 0
    ).

paint(X-Y, Color, PanelsIn, PanelsOut) :-
    ( selectchk(X-Y-_, PanelsIn, Rem)
    ->
        PanelsOut = [X-Y-Color | Rem]
    ;
        PanelsOut = [X-Y-Color | PanelsIn]
    ).

paint_loop(X-Y, DX-DY, ProgramConfig, PanelsIn, PanelsOut) :-
    ProgramConfig = [Input, Index, RelBase, Output],
    camera(X-Y, PanelsIn, Color),
    % TODO: intcode.give_input
    append(Input, [Color], NewInput),
    run(NewInput, RemInput, Index, NewIndex, RelBase, NewRelBase, Output, NewOutput, Halted),
    (
        Halted=true,
        PanelsIn = PanelsOut
    ;
        NewOutput = [SetColor, Direction | RestOutput],
        paint(X-Y, SetColor, PanelsIn, NewPanels),
        left_or_right(Direction, DX-DY, NDX-NDY),
        NX #= X + NDX,
        NY #= Y + NDY,
        NewProgramConfig = [RemInput, NewIndex, NewRelBase, RestOutput],
        paint_loop(NX-NY, NDX-NDY, NewProgramConfig, NewPanels, PanelsOut)
    ).

run_robot(Program, PanelsIn, PanelsOut) :-
    list_to_mem(Program),
    % TODO: intcode.new_program
    ProgramConfig = [ [], 0, 0, [] ],
    paint_loop(0-0, 0-1, ProgramConfig, PanelsIn, PanelsOut),
    clean_mem.

bounds(Panels, MinX-MinY, MaxX-MaxY) :-
    maplist([X-_-_, Z]>>(Z=X), Panels, Xs),
    min_list(Xs, MinX),
    max_list(Xs, MaxX),
    maplist([_-Y-_, Z]>>(Z=Y), Panels, Ys),
    min_list(Ys, MinY),
    max_list(Ys, MaxY).

print(_, _-A, _-Y) :- A #= Y+1.
print(Panels, MinX-MinY, MaxX-MaxY) :-
    Len #= MaxX - MinX + 1,
    length(Xs, Len),
    Xs ins MinX..MaxX,
    chain(Xs, #<),
    all_distinct(Xs),
    maplist({Panels, MaxY}/[In,Out]>>camera(In-MaxY, Panels, Out), Xs, Line),
    maplist([In, Out]>>(In=1->Out='\u2588';Out=' '), Line, Ascii),
    string_chars(Str, Ascii),
    writeln(Str),
    NMaxY #= MaxY - 1,
    print(Panels, MinX-MinY, MaxX-NMaxY).

run :-
    parse(Program),
    run_robot(Program, [], Panels),
    length(Panels, N),
    format("Part 1: ~w~n", [N]),
    run_robot(Program, [0-0-1], PanelsSingleWhite),
    bounds(PanelsSingleWhite, MinX-MinY, MaxX-MaxY),
    print(PanelsSingleWhite, MinX-MinY, MaxX-MaxY).

