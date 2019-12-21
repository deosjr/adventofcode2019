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
    let mut program = intcode::Program::new(&input);

    program.give_ascii_input("NOT D J");
    program.give_ascii_input("NOT J J");
    program.give_ascii_input("NOT C T");
    program.give_ascii_input("AND T J");
    program.give_ascii_input("NOT A T");
    program.give_ascii_input("OR T J");

    program.give_ascii_input("WALK");
    program.run();
    /*
    for out in program.get_all_output() {
        if out == 10 {
            println!();
        } else {
            print!("{}", std::str::from_utf8(&[out as u8]).unwrap());
        }
    }
    */
    println!("Part 1: {}", program.get_all_output().last().unwrap());
}
