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
    x: i32,
    y: i32
}

impl Coord {
    fn new(x: i32, y: i32) -> Coord {
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
}

fn run_robot(brain: &HashMap<i64, i64>, panels: &mut HashMap<Coord, i64>) {
    let mut program = intcode::Program::new(&brain);
    let mut pos = Coord::new(0, 0);
    let mut direction = Coord::new(0, 1);
    while !program.halted() {
        let camera = panels.get(&pos).or(Some(&0)).unwrap();
        program.give_input(*camera);
        program.run();
        let color = program.get_output().unwrap();
        panels.insert(pos, color);
        match program.get_output().unwrap() {
            0 => direction.turn_left(),
            1 => direction.turn_right(),
            _ => panic!()
        };
        pos.x += direction.x;
        pos.y += direction.y;
    }
}

fn main() {
    let input = parse("day11.input").unwrap();
    let mut panels = HashMap::new();
    run_robot(&input, &mut panels);
    println!("Part 1: {}", panels.len());
    let mut panels_single_white = HashMap::new();
    panels_single_white.insert(Coord::new(0,0), 1);
    run_robot(&input, &mut panels_single_white);
    let mut mincoords = Coord::new(std::i32::MAX, std::i32::MAX); 
    let mut maxcoords = Coord::new(std::i32::MIN, std::i32::MIN); 
    for k in panels_single_white.keys() {
        if k.x < mincoords.x {
            mincoords.x = k.x;
        }
        if k.y < mincoords.y {
            mincoords.y = k.y;
        }
        if k.x > maxcoords.x {
            maxcoords.x = k.x;
        }
        if k.y > maxcoords.y {
            maxcoords.y = k.y;
        }
    }
    println!("Part 2: ");
    for y in (mincoords.y..maxcoords.y+1).rev() {
        for x in mincoords.x..maxcoords.x+1 {
            let camera = panels_single_white.get(&Coord::new(x,y)).or(Some(&0)).unwrap();
            match camera {
                0 => print!(" "),
                1 => print!("\u{2588}"),
                _ => panic!()
            }
        }
        println!("");
    }
}
