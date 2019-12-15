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

    fn turn_left(&mut self) {
        let (x,y) = (self.x, self.y);
        self.x = -y;
        self.y = x;
    }

    fn turn_right(&mut self) {
        let (x,y) = (self.x, self.y);
        self.x = y;
        self.y = -x;
    }

    fn plus(self, c: Coord) -> Coord {
        Coord::new(self.x + c.x, self.y + c.y)
    }
}

fn print(grid: &HashMap<Coord, i64>, dists: &HashMap<Coord, u32>) {
    println!("----------------------");
    for y in (-30..30).rev() {
        for x in -30..30 {
            if x == 0 && y == 0 {
                print!("X");
                continue
            }
            let c = grid.get(&Coord::new(x,y)).or(Some(&3)).unwrap();
            match c {
                0 => print!("\u{2588}"),
                1 => {let d = dists.get(&Coord::new(x,y)).unwrap(); print!("{}", d % 10)},
                2 => print!("$"),
                _ => print!("_"),
            }
        }
        println!();
    }
}

fn direction_command(direction: Coord) -> i64 {
    match direction {
        Coord{x: 0, y: 1 } => 1,
        Coord{x: 0, y:-1 } => 2,
        Coord{x:-1, y: 0 } => 3,
        Coord{x: 1, y: 0 } => 4,
        _ => panic!()
    }
}

fn check_wall(direction: Coord, grid: &mut HashMap<Coord, i64>, game: &mut intcode::Program, pos: Coord) -> i64 {
    let d = direction_command(direction);
    game.give_input(d);
    game.run();
    let x = game.get_output().unwrap();
    let newpos = pos.plus(direction);
    grid.insert(newpos.clone(), x);
    x
}

fn update_dist(dist: &mut HashMap<Coord, u32>, current: u32, pos: Coord) -> u32 {
    match dist.get(&pos) {
        Some(&x) => x,
        None => {dist.insert(pos, current+1); current + 1}
    }
}

fn main() {
    let input = parse("day15.input").unwrap();
    let mut game = intcode::Program::new(&input);
    let mut grid = HashMap::new();
    let mut dist = HashMap::new();
    let mut pos = Coord::new(0, 0);
    grid.insert(pos, 1);
    dist.insert(pos, 0);
    let mut direction = Coord::new(0, 1);
    let mut distance = 0;
    let mut maxdist = 0;

    loop {
        match check_wall(direction, &mut grid, &mut game, pos) {
            0 => direction.turn_right(),
            1 => {pos = pos.plus(direction); direction.turn_left(); distance = update_dist(&mut dist, distance, pos)},
            2 => {println!("Part 1: {}", distance + 1); break},
            _ => panic!()
        }
    }

    dist = HashMap::new();
    distance = 0;

    loop {
        match check_wall(direction, &mut grid, &mut game, pos) {
            0 => direction.turn_right(),
            1 => {pos = pos.plus(direction); direction.turn_left(); distance = update_dist(&mut dist, distance, pos)},
            2 => {println!("Part 2: {}", maxdist); break},
            _ => panic!()
        };
        if distance > maxdist {
            maxdist = distance;
        }
    }
}
