use std::fs::File;
use std::io; use std::io::prelude::*;
use std::str::FromStr;
use std::collections::HashMap;

fn parse(filename: &str) -> io::Result<(String, String)> {
    let file = File::open(filename)?;
    let mut lines = io::BufReader::new(file).lines(); let wire1 = lines.next().unwrap()?;
    let wire2 = lines.next().unwrap()?;
    Ok((wire1, wire2))
}

fn map1(wire: String) -> HashMap<(i32,i32),i32> {
    let mut m = HashMap::new();
    m.insert((0,0), 0);
    let instrs = wire.trim_end().split(',');
    let mut x = 0;
    let mut y = 0;
    let mut distance = 0;
    for s in instrs {
        let mut n = String::from(s);
        let instr: String =  n.drain(..1).collect();
        let z = i32::from_str(&*n).unwrap();
        match instr.chars().next().unwrap() {
           'U' => {
                for yy in y..y+z {
                    distance+=1;
                    let coord = (x, yy+1);
                    match m.get(&coord) {
                        Some(d) => (),//distance=*d,
                        None => {m.insert(coord, distance); ()}
                    }
                }
                y = y+z;
               }, 
           'R' => {
                for xx in x..x+z {
                    distance+=1;
                    let coord = (xx+1, y);
                    match m.get(&coord) {
                        Some(d) => (),//distance=*d,
                        None => {m.insert(coord, distance); ()}
                    }
                }
                x = x+z;
               }, 
           'D' => {
                for yy in (y-z+1..y+1).rev() {
                    distance+=1;
                    let coord = (x, yy-1);
                    match m.get(&coord) {
                        Some(d) => (),//distance=*d,
                        None => {m.insert(coord, distance); ()}
                    }
                }
                y = y-z;
               }, 
           'L' => {
                for xx in (x-z+1..x+1).rev() {
                    distance+=1;
                    let coord = (xx-1, y);
                    match m.get(&coord) {
                        Some(d) => (),//distance=*d,
                        None => {m.insert(coord, distance); ()}
                    }
                }
                x = x-z;
               }, 
           _ => panic!()
        }
    }
    m
}

fn main() {
    let (wire1, wire2) = parse("day3.input").unwrap();
    let m1 = map1(wire1);
    let m2 = map1(wire2);
    let mut manhattan = 0;
    for key in m1.keys() {
       let (x, y) = key; 
       if *x==0 && *y==0 {
            continue
       }
       if m2.contains_key(key) {
            let m = x.abs() + y.abs();
            if manhattan == 0 || m < manhattan {
                manhattan = m
            }
       }
    }
    println!("Part 1: {:?}", manhattan);
    let mut delay = 0;
    for key in m1.keys() {
       let (x, y) = key; 
       if *x==0 && *y==0 {
            continue
       }
       if m2.contains_key(key) {
            let d = m1.get(key).unwrap() + m2.get(key).unwrap();
            if delay == 0 || d < delay {
                delay = d
            }
       }
    }
    println!("Part 2: {:?}", delay);
}
