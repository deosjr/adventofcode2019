:- ['intcode.pl'].

parse(Input) :-
    csv_read_file("day13.input", [Row], []),
    Row =.. [_|Input].

update_state([]).
update_state([X, Y, ID|T]) :-
    retractall(grid(X, Y, _)),
    assertz(grid(X, Y, ID)),
    update_state(T).

joystick(X, X, 0).
joystick(PX, BX, 1) :-
    PX #< BX.
joystick(PX, BX, -1) :-
    PX #> BX.

play_game(Config, Score) :-
    run(Config, ConfigAfterRun, Halted),
    get_output(ConfigAfterRun, ConfigAfterOutput, Output),
    update_state(Output),
    (
        Halted=true,
        grid(-1, 0, Score)
    ;
        Halted=false,
        grid(PX, _, 3),
        grid(BX, _, 4),
        joystick(PX, BX, J),
        give_input(J, ConfigAfterOutput, NewConfig),
        play_game(NewConfig, Score)
    ).

run :-
    parse(Program),
    new_program(Program, Config),
    run(Config, [_, _, _, Output], _),
    update_state(Output),
    findall(X-Y, grid(X, Y, 2), Blocks),
    length(Blocks, N),
    format("Part 1: ~w~n", [N]),
    new_program(Program, Config),
    set(0, 2),
    play_game(Config, Score),
    format("Part 2: ~w~n", [Score]).
