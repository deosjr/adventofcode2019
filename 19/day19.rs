use std::fs::File;
use std::io;
use std::io::Read;
use std::str::FromStr;
use std::collections::HashMap;

pub mod intcode;

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

fn in_tractor_beam(input: &HashMap<i64,i64>, x:i64, y:i64) -> bool {
    let mut program = intcode::Program::new(&input);
    program.give_input(x);
    program.give_input(y);
    program.run();
    program.get_output().unwrap() == 1
}

fn main() {
    let input = parse("day19.input").unwrap();
    let mut sum = 0;
    for y in 0..50 {
        for x in 0..50 {
            if in_tractor_beam(&input, x, y) {
                sum += 1;
            }
        }
    }
    println!("Part 1: {}", sum);

    // bounds found by experimentation...
    for yo in 1450..1500 {
        for xo in 1200..1250 {
            if in_tractor_beam(&input, xo+99, yo) && in_tractor_beam(&input, xo, yo+99) {
                println!("Part 2: {}", xo*10000 +  yo);
                return
            }
        }
    }
}
