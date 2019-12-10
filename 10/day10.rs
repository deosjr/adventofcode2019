use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::collections::HashMap;

fn read_file(filename: &str) -> io::Result<io::BufReader<File>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    Ok(reader)
}

fn parse(reader: io::BufReader<File>) -> HashMap<(i32,i32), i32> {
    let mut m = HashMap::new();
    for (y, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        for (x, c) in line.chars().enumerate() {
            match c {
                '#' => m.insert((x as i32, y as i32), 0),
                '.' => continue,
                _   => panic!()
            };
        }
    }
    m
}

fn gcd(x: i32, y: i32) -> i32 {
    if x == 0 {
        y.abs()
    } else {
        gcd(y % x, x)
    }
}

fn collision(m: &HashMap<(i32,i32),i32>, from: (i32,i32), to: (i32,i32), steps: (i32,i32)) -> bool {
    let mut coords = from;
    loop {
        let (x, y) = coords;
        let (dx, dy) = steps;
        coords = (x+dx, y+dy);
        if coords == to {
            return false
        }
        let asteroid = m.get(&coords);
        if asteroid != None {
            return true
        }
    }
}

fn main() {
    let reader = read_file("day10.input").unwrap();
    let m = parse(reader);
    let mut max = 0;
    for (from, _) in &m {
        let mut detected = 0;
        for (to, _) in &m {
            if from == to {
                continue
            }
            let (x, y) = from;
            let (x2, y2) = to;
            let (diffx, diffy) = (x2-x, y2-y);
            let steps = gcd(diffx, diffy);
            let (diffx, diffy) = (diffx/steps, diffy/steps);
            if collision(&m, *from, *to, (diffx, diffy)) {
                continue
            }
            detected += 1;
        }
        if max < detected {
            max = detected;
        }
    }
    println!("Part 1: {}", max);
}
