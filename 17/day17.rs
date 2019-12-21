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

#[derive(PartialEq,Eq,Hash,Debug,Clone,Copy)]
struct Coord {
    x: i64,
    y: i64
}

impl Coord {
    fn new(x: i64, y: i64) -> Coord {
        Coord{x:x, y:y}
    }
}

fn part1(grid: &HashMap<Coord, i64>, maxx: i64, maxy: i64) -> i64 {
    let mut alignment = 0;
    for y in 0..maxy+1 {
        for x in 0..maxx+1 {
            let mut sum = 0;
            sum += grid.get(&Coord::new(x-1, y)).or(Some(&0)).unwrap();
            sum += grid.get(&Coord::new(x+1, y)).or(Some(&0)).unwrap();
            sum += grid.get(&Coord::new(x, y-1)).or(Some(&0)).unwrap();
            sum += grid.get(&Coord::new(x, y+1)).or(Some(&0)).unwrap();
            if sum == 35*4 {
                alignment += x * y;
            }
        }
    }
    alignment
}

fn main() {
    let mut input = parse("day17.input").unwrap();
    let mut p1 = intcode::Program::new(&input);
    let mut grid = HashMap::new();
    p1.run();
    let (mut x, mut y) = (0, 0);
    let mut maxx = -1;
    for out in p1.get_all_output() {
        if out == 10 {
            y += 1;
            if x > maxx {
                maxx=x;
            }
            x = 0;
            continue
        }
        if out == 35 {
            grid.insert(Coord::new(x,y), out);
        }
        x+=1;
    }
    let maxy = y;
    println!("Part 1: {}", part1(&grid, maxx, maxy));

    input.insert(0, 2);
    let mut p2 = intcode::Program::new(&input);

    p2.give_ascii_input("A,B,A,C,A,B,C,A,B,C");
    p2.give_ascii_input("R,8,R,10,R,10");
    p2.give_ascii_input("R,4,R,8,R,10,R,12");
    p2.give_ascii_input("R,12,R,4,L,12,L,12");

    p2.give_input(110);
    p2.give_input(10);

    p2.run();

    println!("Part 2: {}", p2.get_all_output().last().unwrap());
}
