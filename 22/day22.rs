use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

fn read_file(filename: &str) -> Vec<String> {
    let file = File::open(filename).expect("");
    let reader = io::BufReader::new(file);
    reader.lines().map(|l| l.unwrap()).collect::<Vec<String>>()
}


fn main() {
    let input = read_file("day22.input");
    let mut i = 2019;
    let len = 10007;
    for s in input {
        let mut split = s.split(' ');
        let first = split.next().unwrap();
        let last = split.last().unwrap();
        if last == "stack" {
            let pivot = len / 2;
            i = 2*pivot - i;
            continue
        }
        let mut n = i32::from_str(&last).unwrap();
        if first == "cut" {
            if n > 0 {
                if n < i {
                    i -= n;
                } else {
                    i = len - n + i;
                }
            } else {
                n = n.abs();
                if n < len - i {
                    i += n;
                } else {
                    i = n - len + i;
                }
            }
            continue
        }
        if first == "deal" {
            i = (n * i) % len;
        }
    }
    println!("Part 1: {}", i);
}
