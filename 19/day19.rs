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

fn main() {
    let mut input = parse("day19.input").unwrap();
    let mut sum = 0;
    for y in 0..50 {
        for x in 0..50 {
            let mut program = intcode::Program::new(&input);
            program.give_input(x);
            program.give_input(y);
            program.run();
            if program.get_output().unwrap() == 1 {
                sum += 1;
            }
        }
    }
    println!("Part 1: {}", sum);
}
