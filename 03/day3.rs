use std::fs::File;
use std::io; use std::io::prelude::*;
use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
struct Segment {
    fixed_axis: bool, // true for X / false for Y
    fixed: i32,
    min: i32,
    max: i32
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
    let mut v: Vec<Segment> = Vec::new();
    for s in instrs {
        let mut n = String::from(s);
        let instr: String =  n.drain(..1).collect();
        let z = i32::from_str(&*n).unwrap();
        match instr.chars().next().unwrap() {
           'U' => {v.push(Segment{fixed_axis:true, fixed:x, min:y, max:y+z}); y=y+z}, 
           'R' => {v.push(Segment{fixed_axis:false, fixed:y, min:x, max:x+z}); x=x+z}, 
           'D' => {v.push(Segment{fixed_axis:true, fixed:x, min:y-z, max:y}); y=y-z}, 
           'L' => {v.push(Segment{fixed_axis:false, fixed:y, min:x-z, max:x}); x=x-z}, 
           _ => panic!()
        }
    }
    v
}

fn collision(w: Segment, v: Segment) -> i32 {
    if w.fixed_axis == v.fixed_axis {
        if w.fixed == v.fixed {
            // overlap on same line?
        }
        return -1 
    }
    if w.min < v.fixed && w.max > v.fixed && v.min < w.fixed && v.max > w.fixed {
        return w.fixed.abs() + v.fixed.abs()
    }
    -1
}

fn main() {
    let (wire1, wire2) = parse("day3.input").unwrap();
    let mut manhattan = -1;
    for w in wire1 {
        for v in &wire2 {
            let c = collision(w,*v);
            if c == -1 || c == 0 {
                continue; 
            }
            if manhattan == -1 || c < manhattan {
                manhattan = c;
            }
        }
    }
    println!("Part 1: {}", manhattan);
}
