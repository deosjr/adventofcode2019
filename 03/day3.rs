use std::fs::File;
use std::io; use std::io::prelude::*;
use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
struct Segment {
    fixed_axis: bool, // true for X / false for Y
    distance_start: i32,
    fixed: i32,
    begin: i32,
    end: i32
}

fn parse(filename: &str) -> io::Result<(Vec<Segment>,Vec<Segment>)> {
    let file = File::open(filename)?;
    let mut lines = io::BufReader::new(file).lines(); let wire1 = lines.next().unwrap()?;
    let wire2 = lines.next().unwrap()?;
    Ok((wire_to_segments(wire1.trim_end()), wire_to_segments(wire2.trim_end())))
}

fn wire_to_segments(wire: &str) -> Vec<Segment> {
    let instrs = wire.split(',');
    let mut x = 0;
    let mut y = 0;
    let mut distance = 0;
    let mut v: Vec<Segment> = Vec::new();
    for s in instrs {
        let mut n = String::from(s);
        let instr: String =  n.drain(..1).collect();
        let z = i32::from_str(&*n).unwrap();
        match instr.chars().next().unwrap() {
           'U' => {v.push(Segment{fixed_axis:true, distance_start:distance, fixed:x, begin:y, end:y+z}); y=y+z}, 
           'R' => {v.push(Segment{fixed_axis:false, distance_start:distance, fixed:y, begin:x, end:x+z}); x=x+z}, 
           'D' => {v.push(Segment{fixed_axis:true, distance_start:distance, fixed:x, begin:y, end:y-z}); y=y-z}, 
           'L' => {v.push(Segment{fixed_axis:false, distance_start:distance, fixed:y, begin:x, end:x-z}); x=x-z}, 
           _ => panic!()
        }
        distance += z;
    }
    v
}

fn collision(w: Segment, v: Segment, result_func:fn(Segment,Segment)->i32) -> i32 {
    if w.fixed_axis == v.fixed_axis {
        if w.fixed == v.fixed {
            // overlap on same line?
        }
        return -1 
    }
    if ((w.begin < v.fixed && w.end > v.fixed) || (w.end < v.fixed && w.begin > v.fixed)) 
        && ((v.begin < w.fixed && v.end > w.fixed) || (v.end < w.fixed && v.begin > w.fixed)) {
        return result_func(w, v)
    }
    -1
}

fn result1(w: Segment, v: Segment) -> i32 {
    w.fixed.abs() + v.fixed.abs()
}

fn part1(m1: &Vec<Segment>, m2: &Vec<Segment>) -> i32 {
    main_loop(m1, m2, result1)
}

fn result2(w: Segment, v: Segment) -> i32 {
    w.distance_start + (v.fixed - w.begin).abs() + v.distance_start + (w.fixed - v.begin).abs()
}

fn part2(m1: &Vec<Segment>, m2: &Vec<Segment>) -> i32 {
    main_loop(m1, m2, result2)
}

fn main_loop(m1: &Vec<Segment>, m2: &Vec<Segment>, result_func:fn(Segment,Segment)->i32) -> i32 {
    let mut delay = -1;
    for w in m1 {
        for v in m2 {
            let c = collision(*w,*v,result_func);
            if c == -1 || c == 0 {
                continue; 
            }
            if delay == -1 || c < delay {
                delay = c;
            }
        }
    }
    delay
}

fn main() {
    let (wire1, wire2) = parse("day3.input").unwrap();
    println!("Part 1: {}", part1(&wire1, &wire2));
    println!("Part 2: {}", part2(&wire1, &wire2));
}
