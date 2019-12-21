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

fn main() {
    let input = parse("day21.input").unwrap();
    let mut part1 = intcode::Program::new(&input);

    part1.give_ascii_input("NOT D J");
    part1.give_ascii_input("NOT J J");
    part1.give_ascii_input("NOT C T");
    part1.give_ascii_input("AND T J");
    part1.give_ascii_input("NOT A T");
    part1.give_ascii_input("OR T J");

    part1.give_ascii_input("WALK");
    part1.run();
    println!("Part 1: {}", part1.get_all_output().last().unwrap());

    let mut part2 = intcode::Program::new(&input);

    // (E or H) and (D and not C)
    part2.give_ascii_input("NOT E J");
    part2.give_ascii_input("NOT J J");
    part2.give_ascii_input("OR H J");
    part2.give_ascii_input("AND D J");
    part2.give_ascii_input("NOT C T");
    part2.give_ascii_input("AND T J");

    // OR not B and D and H
    part2.give_ascii_input("NOT B T");
    part2.give_ascii_input("AND D T");
    part2.give_ascii_input("AND H T");
    part2.give_ascii_input("OR T J");

    // OR not A
    part2.give_ascii_input("NOT A T");
    part2.give_ascii_input("OR T J");

    part2.give_ascii_input("RUN");
    part2.run();
    println!("Part 2: {}", part2.get_all_output().last().unwrap());
}
