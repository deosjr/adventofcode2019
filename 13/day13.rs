use std::fs::File;
use std::io;
use std::io::Read;
use std::str::FromStr;
use std::collections::HashMap;

mod intcode;

fn parse(filename: &str) -> io::Result<HashMap<i64,i64>> {
    let mut file = File::open(filename)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    Ok(str_to_map(&input))
}

fn str_to_map(input: &str) -> HashMap<i64,i64> {
    let mut m = HashMap::new();
    for (i, x) in input.trim_end().split(',').map(|x| i64::from_str(&x).unwrap()).enumerate() {
        m.insert(i as i64, x);
    }
    m
}

#[derive(PartialEq,Eq,Hash,Debug,Clone,Copy)]
struct Coord {
    x: i64,
    y: i64
}

impl Coord {
    fn new(x: i64, y: i64) -> Coord {
        Coord{x:x, y:y}
    }
}

fn game_state(state: Vec<i64>) -> (Coord, Coord, i64) {
    let (mut paddle, mut ball, mut score) = (Coord::new(-1,-1), Coord::new(-1,-1), 0);
    for out in state.chunks_exact(3) {
        let (x, y, value) = (out[0], out[1], out[2]);
        if x == -1 && y == 0 {
            score = value;
            continue
        }
        if value == 3 {
            paddle = Coord::new(x,y);
            continue
        }
        if value == 4 {
            ball = Coord::new(x,y);
        }
    }
    (paddle, ball, score)
}

fn main() {
    let mut input = parse("day13.input").unwrap();
    let mut game = intcode::Program::new(&input);
    let mut tiles = HashMap::new();
    game.run();
    for out in game.get_all_output().chunks_exact(3) {
        let (x, y, tileid) = (out[0], out[1], out[2]);
        tiles.insert(Coord::new(x,y), tileid);
    }
    let blocks = tiles.values().filter(|&x| *x==2).count();
    println!("Part 1: {}", blocks);

    input.insert(0, 2);
    let mut newgame = intcode::Program::new(&input);
    let mut score = 0;
    while !newgame.halted() {
        newgame.run();
        let out = newgame.get_all_output();        
        let (paddle, ball, s) = game_state(out);
        score = s;
        if paddle.x < ball.x {
            newgame.give_input(1);
        } else if paddle.x > ball.x {
            newgame.give_input(-1);
        } else {
            newgame.give_input(0);
        }
    }
    println!("Part 2: {}", score);
}
