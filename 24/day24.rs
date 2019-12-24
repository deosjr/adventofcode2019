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

fn part1(input: &Vec<u8>) -> i32 {
    let mut state = input.clone();
    let mut m = HashMap::new();
    m.insert(biodiversity(&input), true);
    loop {
        state = step(state);
        let b = biodiversity(&state);
        match m.insert(b, true) {
            Some(_) => return b,
            None => continue
        }
    }
}

fn neighbours2(state: &Vec<u8>, index: usize, down: &Vec<u8>, up: &Vec<u8>) -> u8 {
    // size is 5x5
    let mut sum = 0;
    // NORTH
    if index == 17 {
        for i in 0..5 {
            sum += down[i+20];
        }
    } else if index >= 5 {
        sum += state[index-5];
    } else {
        sum += up[7];
    }
    // SOUTH
    if index == 7 {
        for i in 0..5 {
            sum += down[i];
        }
    } else if index < 20 {
        sum += state[index+5];
    } else {
        sum += up[17];
    }
    // WEST
    if index == 13 {
        for i in 0..5 {
            sum += down[i*5 + 4];
        }
    } else if index % 5 != 0 {
        sum += state[index-1];
    } else {
        sum += up[11];
    }
    // EAST
    if index == 11 {
        for i in 0..5 {
            sum += down[i*5];
        }
    } else if index % 5 != 4 {
        sum += state[index+1];
    } else {
        sum += up[13]
    }
    sum
}

fn step3(state: &Vec<u8>, down: &Vec<u8>, up: &Vec<u8>) -> Vec<u8> {
    let mut newstate = state.clone();
    for (i, x) in newstate.iter_mut().enumerate() {
        if i == 12 {
            continue
        }
        let n = neighbours2(&state, i, down, up);
        match *x {
            1 => *x = if n == 1 { 1 } else { 0 },
            0 => *x = if n == 1 || n == 2 { 1 } else { 0 },
            _ => panic!()
        }
    }
    newstate
}

fn step2(states: &HashMap<i32, Vec<u8>>, key: i32) -> Vec<u8> {
    let state = states.get(&key).unwrap();
    if key == -103 || key == 103 {
        return state.clone();
    }
    step3(state, states.get(&(key-1)).unwrap(), states.get(&(key+1)).unwrap())
}

fn step_states(states: HashMap<i32, Vec<u8>>) -> HashMap<i32, Vec<u8>> {
    let mut newstates = states.clone();
    for k in states.keys() {
        newstates.insert(*k, step2(&states, *k));
    }
    newstates
}

fn count_bugs(states: &HashMap<i32, Vec<u8>>) -> i32 {
    let mut sum = 0;
    for state in states.values() {
        sum += state.iter().map(|&x| x as i32).sum::<i32>();
    }
    sum
}

fn part2(input: &Vec<u8>) -> i32 {
    let mut states = HashMap::new();
    // there will be exactly n/2 + 1 layers to consider after n minutes
    // and those will need neighbours to check so lets init n/2 + 3
    for i in -103..104 {
        states.insert(i, vec![0;25]);
    }
    let mut start_state = input.clone();
    // make sure middle is empty
    start_state[12] = 0;
    states.insert(0, start_state);
    for _ in 0..200 {
        states = step_states(states);
    }
    count_bugs(&states)
}

fn main() {
    let input = parse("day24.input").unwrap();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
