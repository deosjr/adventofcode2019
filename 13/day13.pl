:- ['intcode.pl'].

parse(Input) :-
    csv_read_file("day13.input", [Row], []),
    Row =.. [_|Input].

update_state([]).
update_state([X, Y, ID|T]) :-
    assertz(grid(X, Y, ID)),
    update_state(T).

run :-
    parse(Program),
    new_program(Program, Config),
    run(Config, [_, _, _, Output], _),
    update_state(Output),
    % TODO: I dont think we need to retract after part1
    findall(X-Y, grid(X, Y, 2), Blocks),
    length(Blocks, N),
    format("Part 1: ~w~n", [N]).
