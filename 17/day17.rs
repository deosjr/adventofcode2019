use std::fs::File;
use std::io;
use std::io::Read;
use std::str::FromStr;
use std::collections::HashMap;

mod intcode;

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
    let (mut maxx, mut maxy) = (-1, -1);
    for out in p1.get_all_output() {
        if out == 10 {
            y += 1;
            if x > maxx {
                maxx=x;
            }
            x = 0;
            println!();
            continue
        }
        if out == 35 {
            grid.insert(Coord::new(x,y), out);
        }
        x+=1;
        print!("{}", std::str::from_utf8(&[out as u8]).unwrap())
    }
    maxy = y;
    println!("{},{}", maxx, maxy);
    println!("Part 1: {}", part1(&grid, maxx, maxy));

    input.insert(0, 2);
    let mut p2 = intcode::Program::new(&input);

    // A B A C A B C A B C
    let main = vec![65, 44, 66, 44, 65, 44, 67, 44, 65, 44, 66, 44, 67, 44, 65, 44, 66, 44, 67, 10]; 
    // R 8 R 1 0 R 1 0
    let a = vec![82, 44, 56, 44, 82, 44, 49, 48, 44, 82, 44, 49, 48, 10]; 
    // R 4 R 8 R 1 0 R 1 2
    let b = vec![82, 44, 52, 44, 82, 44, 56, 44, 82, 44, 49, 48, 44, 82, 44, 49, 50, 10]; 
    // R 1 2 R 4 L 1 2 L 1 2
    let c = vec![82, 44, 49, 50, 44, 82, 44, 52, 44, 76, 44, 49, 50, 44, 76, 44, 49, 50, 10]; 

    for x in main.iter() {
        p2.give_input(*x);
    }
    for x in a.iter() {
        p2.give_input(*x);
    }
    for x in b.iter() {
        p2.give_input(*x);
    }
    for x in c.iter() {
        p2.give_input(*x);
    }

    p2.give_input(110);
    p2.give_input(10);
    /*
    p2.give_input(65);
    p2.give_input(10);
    p2.give_input(82);
    p2.give_input(10);
    p2.give_input(82);
    p2.give_input(10);
    p2.give_input(82);
    p2.give_input(10);
    p2.give_input(110);
    p2.give_input(10);
    */

    p2.run();

    for out in p2.get_all_output() {
        if out == 10 {
            println!();
            continue
        }
        match std::str::from_utf8(&[out as u8]) {
            Ok(s) => print!("{}", s),
            Err(_) => println!("{}", out)
        }
    }
}
