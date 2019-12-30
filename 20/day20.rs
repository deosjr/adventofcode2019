use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::collections::HashMap;

mod path;

fn parse(filename: &str) -> Vec<String> {
    let file = File::open(filename).expect("");
    let reader = io::BufReader::new(file);
    reader.lines().map(|l| l.unwrap()).collect::<Vec<String>>() }

#[derive(Ord,PartialOrd,Eq,PartialEq,Hash,Copy,Clone)]
struct Point {
    x: i64,
    y: i64
}

impl Point {
    fn new(x: i64, y: i64) -> Point {
        Point{x: x, y: y}
    }
}

fn von_neumann_neighbours(p: Point) -> [Point; 4] {
    [ Point::new(p.x + 1, p.y),
      Point::new(p.x - 1, p.y),
      Point::new(p.x, p.y + 1),
      Point::new(p.x, p.y - 1) ]
}

struct DonutP1<'a> {
    coords: &'a HashMap<Point,Tile>
}

impl<'a> path::Map for DonutP1<'a> {
    type Node = Point; 
    fn neighbours(&self, n: Self::Node) -> Vec<Self::Node> {
        let mut neighbours: Vec<Self::Node> = vec![];
        for p in von_neumann_neighbours(n).iter() {
            if self.coords.contains_key(&p) {
                neighbours.push(*p);
            }
        }
        match self.coords.get(&n).unwrap().warp {
            Some(p) => neighbours.push(p),
            None => (),
        }
        neighbours
    }

    fn g(&self, _n: Self::Node, _neighbour: Self::Node) -> i64 { 1 }

    fn h(&self, n: Self::Node, goal: Self::Node) -> i64 {
        let dx = goal.x - n.x;
        let dy = goal.y - n.y;
        dx.abs() + dy.abs()
    }
}

struct DonutP2<'a> {
    coords: &'a HashMap<Point,Tile>
}

#[derive(Ord,PartialOrd,Eq,PartialEq,Hash,Copy,Clone)]
struct PointLevel {
    point: Point,
    level: i32
}

impl PointLevel {
    fn new(p: Point, l: i32) -> PointLevel {
        PointLevel{point: p, level: l}
    }
}

impl<'a> path::Map for DonutP2<'a> {
    type Node = PointLevel; 
    fn neighbours(&self, n: Self::Node) -> Vec<Self::Node> {
        let mut neighbours: Vec<Self::Node> = vec![];
        for p in von_neumann_neighbours(n.point).iter() {
            if self.coords.contains_key(&p) {
                neighbours.push(PointLevel::new(*p, n.level));
            }
        }
        let t = self.coords.get(&n.point).unwrap();
        match t.warp {
            Some(p) => {
                if t.outer {
                    if n.level == 0 {
                        return neighbours;
                    }
                    neighbours.push(PointLevel::new(p, n.level-1));
                } else {
                    neighbours.push(PointLevel::new(p, n.level+1));
                }
            },
            None => (),
        }
        neighbours
    }

    fn g(&self, _n: Self::Node, _neighbour: Self::Node) -> i64 { 1 }

    fn h(&self, n: Self::Node, goal: Self::Node) -> i64 {
        let dx = goal.point.x - n.point.x;
        let dy = goal.point.y - n.point.y;
        // going deeper should be more costly (100 is arbitrary)
        dx.abs() + dy.abs() + (n.level as i64) * 100
    }
}

struct Tile {
    warp:  Option<Point>,
    outer: bool
}

impl Tile {
    fn new() -> Tile {
        Tile{warp: None, outer: false}
    }
}

fn connect_portals(m: &mut HashMap<Point,Tile>, labels: &mut HashMap<String,Point>, label: String, p1: Point, outer: bool) {
    match labels.get(&label) {
        Some(&p2) => {
            m.insert(p1, Tile{warp: Some(p2), outer: outer});
            m.insert(p2, Tile{warp: Some(p1), outer: !outer});
        },
        None => {labels.insert(label, p1); ()}
    }
}

fn main() {
    let grid = parse("day20.input");
    let xmid = grid.len() / 2;
    let mut yoff = 0;
    loop {
        let c = grid[yoff+2].chars().nth(xmid).unwrap();
        if c != '.' && c != '#' {
            break
        }
        yoff += 1;
    }

    let outerleft = 2;
    let outertop = 2;
    let outerright = grid[0].len() - 3;
    let outerbottom = grid.len() - 3;

    let innerleft = outerleft + yoff - 1;
    let innertop = outertop + yoff - 1;
    let innerright = outerright - yoff + 1;
    let innerbottom = outerbottom - yoff + 1;
    
    let mut m: HashMap<Point,Tile> = HashMap::new();
    let mut labels: HashMap<String,Point> = HashMap::new();
    for (y, line) in grid.iter().enumerate() {
        for (x, r) in line.chars().enumerate() {
            if r == '#' || r == ' ' {
                continue
            }
            let p = Point::new(x as i64, y as i64);
            if r == '.' {
                m.insert(p, Tile::new());
            }
            if y > innertop && y < innerbottom {
                if x == outerleft || x == innerright {
                    // read portal label to the left
                    let l1 = grid.get(y).unwrap().chars().nth(x-2).unwrap();
                    let l2 = grid.get(y).unwrap().chars().nth(x-1).unwrap();
                    let label = format!("{}{}", l1, l2);
                    connect_portals(&mut m, &mut labels, label, p, x == outerleft);
                    continue
                }
                if x == outerright || x == innerleft {
                    // read portal label to the right
                    let l1 = grid.get(y).unwrap().chars().nth(x+1).unwrap();
                    let l2 = grid.get(y).unwrap().chars().nth(x+2).unwrap();
                    let label = format!("{}{}", l1, l2);
                    connect_portals(&mut m, &mut labels, label, p, x == outerright);
                    continue
                }
            }
            if x > innerleft && x < innerright {
                if y == outertop || y == innerbottom {
                    // read portal label above
                    let l1 = grid.get(y-2).unwrap().chars().nth(x).unwrap();
                    let l2 = grid.get(y-1).unwrap().chars().nth(x).unwrap();
                    let label = format!("{}{}", l1, l2);
                    connect_portals(&mut m, &mut labels, label, p, y == outertop);
                    continue
                }
                if y == outerbottom || y == innertop {
                    // read portal label below
                    let l1 = grid.get(y+1).unwrap().chars().nth(x).unwrap();
                    let l2 = grid.get(y+2).unwrap().chars().nth(x).unwrap();
                    let label = format!("{}{}", l1, l2);
                    connect_portals(&mut m, &mut labels, label, p, y == outerbottom);
                }
            }
        }
    }

    let dm = DonutP1{coords: &m};
    let start = *(labels.get("AA").unwrap());
    let end = *(labels.get("ZZ").unwrap());
    println!("Part 1: {}", path::find_route(dm, start, end).unwrap().len() - 1);

    let dm2 = DonutP2{coords: &m};
    let start2 = PointLevel::new(start, 0);
    let end2 = PointLevel::new(end, 0);
    println!("Part 2: {}", path::find_route(dm2, start2, end2).unwrap().len() - 1);
}
