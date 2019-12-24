use std::fs::File;
use std::io;
use std::io::Read;
use std::collections::HashMap;

fn parse(filename: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filename)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let mut v = vec![];
    for line in input.split('\n') {
        for r in line.chars() {
            if r == '#' {
                v.push(1);
            } else {
                v.push(0);
            }
        }
    }
    Ok(v)
}

fn neighbours(state: &Vec<u8>, index: usize) -> u8 {
    // size is 5x5
    let mut sum = 0;
    if index >= 5 {
        sum += state[index-5];
    }
    if index < 20 {
        sum += state[index+5];
    }
    if index % 5 != 0 {
        sum += state[index-1];
    }
    if index % 5 != 4 {
        sum += state[index+1];
    }
    sum
}

fn step(state: Vec<u8>) -> Vec<u8> {
    let mut newstate = state.clone();
    for (i, x) in newstate.iter_mut().enumerate() {
        let n = neighbours(&state, i);
        match *x {
            1 => *x = if n == 1 { 1 } else { 0 },
            0 => *x = if n == 1 || n == 2 { 1 } else { 0 },
            _ => panic!()
        }
    }
    newstate
}

fn biodiversity(state: &Vec<u8>) -> i32 {
    state.iter().enumerate().map(|(i, &x)| x as i32 * 2_i32.pow(i as u32)).sum()
}

fn print(state: &Vec<u8>) {
    for c in state.chunks(5) {
        let line = c.iter().map(|&x| if x == 1 { '#' } else { '.' }).collect::<String>();
        println!("{}", line);
    }
    println!();
}

fn main() {
    let input = parse("day24.input").unwrap();
    let mut state = input.clone();
    let mut m = HashMap::new();
    m.insert(biodiversity(&input), true);
    loop {
        state = step(state);
        let b = biodiversity(&state);
        match m.insert(b, true) {
            Some(_) => {println!("{}", b); break},
            None => continue
        }
    }
    //print(&state);
}
