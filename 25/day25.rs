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
    let raw = parse("day25.input").unwrap();
    let mut game = intcode::Program::new(&raw);

    let mut input = String::new();
    loop {
        game.run();
        for c in game.get_all_output() {
            print!("{}", std::char::from_u32(c as u32).unwrap());
        }
        match io::stdin().read_line(&mut input) {
            Ok(n) => input = input.trim_end().to_string(),
            Err(error) => println!("error: {}", error),
        }
        game.give_ascii_input(&input);
        input = "".to_string();
    }
}
