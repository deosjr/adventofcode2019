use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

fn read_file(filename: &str) -> Vec<(fn(i64,i64,i64)->i64, i64)> {
    let file = File::open(filename).expect("");
    let reader = io::BufReader::new(file);
    let mut f: Vec<(fn(i64,i64,i64)->i64, i64)> = vec![];
    for line in reader.lines() {
        let line = line.unwrap();
        let mut split = line.split(' ');
        let first = split.next().unwrap();
        let last = split.last().unwrap();
        if last == "stack" {
            f.push( (stack, 0) );
            continue
        }
        let n = i64::from_str(&last).unwrap();
        if first == "cut" {
            f.push( (cut, n) );
            continue
        }
        if first == "deal" {
            f.push( (deal, n) );
        }
    }
    f
}

fn stack(index: i64, len: i64, _: i64) -> i64 {
    len - 1 - index
}

fn cut(index: i64, len: i64, mut n: i64) -> i64 {
    if n < 0 {
        n += len;
    }
    let mut ans = index - n;
    if n >= index {
        ans += len;
    }
    ans
}

fn deal(index: i64, len: i64, n: i64) -> i64 {
    (n * index) % len
}

fn shuffle(index: i64, len: i64, shufflefuncs: &Vec<(fn(i64,i64,i64)->i64, i64)>) -> i64 {
    let mut i = index;
    for (f, n) in shufflefuncs {
        i = f(i, len, *n);
    }
    i
}

fn main() {
    let shufflefuncs = read_file("day22.input");
    println!("Part 1: {}", shuffle(2019, 10007, &shufflefuncs));

    //let times: i64 = 101741582076661;
    let mut i = 2020;
    for _ in 0..10 {
        i = shuffle(i, 119315717514047, &shufflefuncs);
        println!("{} ", i);
    }
    //println!("Part 2: {}", i);
}
