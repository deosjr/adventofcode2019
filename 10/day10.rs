use std::fs::File;
use std::io;
use std::io::prelude::*; use std::collections::HashMap;

#[derive(PartialEq,Eq,Hash,Debug,Clone,Copy)]
struct Coord {
    x: i32,
    y: i32
}

impl Coord {
    fn new(x: i32, y: i32) -> Coord {
        Coord{x:x, y:y}
    }

    fn length(self) -> f64 {
        ((self.x*self.x + self.y*self.y) as f64).sqrt()
    }

    fn order(self) -> i32 {
        let unit_y = self.y as f64 / self.length();
        let acos = unit_y.acos();
        let o = if self.x >= 0 { acos  } else {  -acos  };
        // we only need to check collisions up to a few decimals
        (o * 10000.) as i32
    }
}

fn read_file(filename: &str) -> io::Result<io::BufReader<File>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    Ok(reader)
}

fn parse(reader: io::BufReader<File>) -> HashMap<Coord, i32> {
    let mut m = HashMap::new();
    for (y, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        for (x, c) in line.chars().enumerate() {
            match c {
                '#' => m.insert(Coord::new(x as i32, y as i32), 0),
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

fn collision(m: &HashMap<Coord,i32>, from: Coord, to: Coord, steps: Coord) -> bool {
    let mut coords = from;
    loop {
        coords.x += steps.x;
        coords.y += steps.y;
        if coords == to {
            return false
        }
        let asteroid = m.get(&coords);
        if asteroid != None {
            return true
        }
    }
}

// TODO: map is never updated cause borrowing is hard :(
fn part1(m: &HashMap<Coord, i32>) -> (Coord, i32) {
    let mut max = 0;
    let mut station = Coord::new(-1, -1);
    for (&from, _) in m.iter() {
        let mut detected = 0;
        for (&to, _) in m.iter() {
            if from == to {
                continue
            }
            let mut diff = Coord::new(to.x-from.x, to.y-from.y);
            let steps = gcd(diff.x, diff.y);
            diff.x = diff.x/steps;
            diff.y = diff.y/steps;
            if collision(&m, from, to, diff) {
                continue
            }
            detected += 1;
        }
        if max < detected {
            max = detected;
            station = from;
        }
    }
    (station, max)
}

fn part2(m: &HashMap<Coord, i32>, station: Coord) -> i32 {
    let mut order: HashMap<i32, Vec<Coord>> = HashMap::new();
    for (&asteroid, _) in m.iter() {
        if asteroid == station {
            continue
        }
        let relative = Coord::new(asteroid.x - station.x, asteroid.y - station.y);
        let o = relative.order();
        if let Some(v) = order.get_mut(&o) {
            v.push(relative);
            v.sort_by(|a, b| (a.x.abs()+a.y.abs()).cmp(&(b.x.abs()+b.y.abs())).reverse());
        } else {
            order.insert(o, vec![relative]);
        }
    }

    let mut keys = order.keys().map(|&x| x).collect::<Vec<i32>>();
    keys.sort();
    keys.reverse();
    let mut i = 0;
    for k in keys.iter().cycle() {
        let v = order.get_mut(&k).unwrap();
        if v.len() == 0 {
            continue
        }
        let asteroid = v.pop().unwrap();
        i += 1;
        if i == 200 {
            return (asteroid.x + station.x) * 100 + asteroid.y + station.y;
        }
    }
    -1
}

fn main() {
    let reader = read_file("day10.input").unwrap();
    let m = parse(reader);
    let (station, out1) = part1(&m);
    println!("Part 1: {}", out1);
    println!("Part 2: {}", part2(&m, station));
}
