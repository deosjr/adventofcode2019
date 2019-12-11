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

run :-
    parse(Program),
    run_robot(Program, [], Panels),
    length(Panels, N),
    format("Part 1: ~w~n", [N]).
