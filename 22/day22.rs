use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

fn read_file(filename: &str, len: i128) -> (Vec<(fn(i128,i128,i128)->i128, i128)>, Vec<(fn(i128,i128,i128)->i128, i128)>) {
    let file = File::open(filename).expect("");
    let reader = io::BufReader::new(file);
    let mut f: Vec<(fn(i128,i128,i128)->i128, i128)> = vec![];
    let mut rev: Vec<(fn(i128,i128,i128)->i128, i128)> = vec![];
    for line in reader.lines() {
        let line = line.unwrap();
        let mut split = line.split(' ');
        let first = split.next().unwrap();
        let last = split.last().unwrap();
        if last == "stack" {
            f.push( (stack, 0) );
            rev.push( (stack, 0) );
            continue
        }
        let n = i128::from_str(&last).unwrap();
        if first == "cut" {
            if n < 0 {
                f.push( (cut, n+len) );
                rev.push( (cut, -n) );
            } else {
                f.push( (cut, n) );
                rev.push( (cut, len-n) );
            }
            continue
        }
        if first == "deal" {
            f.push( (deal, n) );
            rev.push( (deal, mmi(n, len)) );
        }
    }
    (f, rev.iter().rev().map(|&f| f).collect())
}

fn stack(index: i128, len: i128, _: i128) -> i128 {
    -1*index -1 % len
}

fn cut(index: i128, len: i128, n: i128) -> i128 {
    (index - n) % len
}

fn deal(index: i128, len: i128, n: i128) -> i128 {
    (n * index) % len
}

fn shuffle(index: i128, len: i128, shufflefuncs: &Vec<(fn(i128,i128,i128)->i128, i128)>) -> i128 {
    let mut i = index;
    for (f, n) in shufflefuncs {
        i = f(i, len, *n);
    }
    (i + len) % len
}

fn cut_rev(index: i128, len: i128, n: i128) -> i128 {
    (index + n) % len
}

fn deal_rev(index: i128, len: i128, n: i128) -> i128 {
    (mmi(n, len) * index) % len
}

// stolen from rosetta code
fn mmi(a: i128, module: i128) -> i128 {
  let mut mn = (module, a);
  let mut xy = (0, 1);
 
  while mn.1 != 0 {
    xy = (xy.1, xy.0 - (mn.0 / mn.1) * xy.1);
    mn = (mn.1, mn.0 % mn.1);
  }
 
  while xy.0 < 0 {
    xy.0 += module;
  }
  xy.0
}

fn shuffle_rev(index: i128, len: i128, shufflefuncs: &Vec<(fn(i128,i128,i128)->i128, i128)>) -> i128 {
    let mut i = index;
    for (f, n) in shufflefuncs.iter().rev() {
        match f{
            f if *f == stack => i = stack(i, len, *n),
            f if *f == cut => i = cut_rev(i, len, *n),
            f if *f == deal => i = deal_rev(i, len, *n),
            _ => panic!()
        }
    }
    (i + len) % len
}

fn main() {
    let (shufflefuncs, revfuncs) = read_file("day22.input", 10007);
    println!("Part 1: {}", shuffle(2019, 10007, &shufflefuncs));

    let mut i = 3143;
    i = shuffle_rev(i, 10007, &shufflefuncs);
    println!("{} ", i);
    i = 3143;
    i = shuffle(i, 10007, &revfuncs);
    println!("{} ", i);

    let size: i128 = 119315717514047;
    let (_, revfuncs2) = read_file("day22.input", size);
    let (mut a, mut b) = (1, 0);
    for (f, n) in revfuncs2.iter() {
        match f{
            f if *f == stack => {a = -a; b = -b-1},
            f if *f == cut => b = (b - n) % size,
            f if *f == deal => {a = (a * n) % size; b = (b * n) % size},
            _ => panic!()
        };
        if a < 0 {
            a += size;
        }
        if b < 0 {
            b += size;
        }
    }
    println!("{}x + {}", a, b);
    //108816453867770x + 57885164799294
    i = 2020;
    i = shuffle(i, size, &revfuncs2);
    println!("{}", i);
    i = 2020;
    println!("{}", (a*i + b) % size);

    let times: i128 = 101741582076661;
    let atimes: i128 = 108590384676962; //thanks wolframalpha
    i = (atimes * i) % size;
    let j = ((atimes-1) * mmi(a-1, size) ) % size;
    i = (i + b*j) % size; 

    println!("Part 2: {}", i);
}
