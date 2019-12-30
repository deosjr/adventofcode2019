use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::collections::HashMap;

mod path;

fn parse(filename: &str) -> Vec<String> {
    let file = File::open(filename).expect("");
    let reader = io::BufReader::new(file);
    reader.lines().map(|l| l.unwrap()).collect::<Vec<String>>() }

#[derive(Debug,Ord,PartialOrd,Eq,PartialEq,Hash,Copy,Clone)]
struct NodeP1 {
    x: i64,
    y: i64
}

impl NodeP1 {
    fn new(x: i64, y: i64) -> NodeP1 {
        NodeP1{x: x, y: y}
    }
}

struct Donut {
    coords: HashMap<NodeP1,Tile>
}

impl path::Map for Donut {
    type Node = NodeP1; 
    fn neighbours(&self, n: Self::Node) -> Vec<Self::Node> {
        let mut neighbours: Vec<Self::Node> = vec![];
        let npoints = [ NodeP1::new(n.x + 1, n.y),
                        NodeP1::new(n.x - 1, n.y),
                        NodeP1::new(n.x, n.y + 1),
                        NodeP1::new(n.x, n.y - 1) ];
        for p in npoints.iter() {
            match self.coords.get(&p) {
                Some(t) => {
                    neighbours.push(t.point);
                },
                None => continue
            }
        }
        let t = self.coords.get(&n).unwrap();
        if t.warp.x != 0 && t.warp.y != 0 {
            neighbours.push(t.warp);
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

struct Tile {
    point: NodeP1,
    warp:  NodeP1
}

impl Tile {
    fn new(p: NodeP1) -> Tile {
        Tile{point: p, warp: NodeP1::new(0,0)}
    }
}

fn connect_portals(m: &mut HashMap<NodeP1,Tile>, labels: &mut HashMap<String,NodeP1>, label: String, p1: NodeP1) {
    match labels.get(&label) {
        Some(&p2) => {
            m.insert(p1, Tile{point: p1, warp: p2});
            m.insert(p2, Tile{point: p2, warp: p1});
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
    
    let mut m: HashMap<NodeP1,Tile> = HashMap::new();
    let mut labels: HashMap<String,NodeP1> = HashMap::new();
    for (y, line) in grid.iter().enumerate() {
        for (x, r) in line.chars().enumerate() {
            if r == '#' || r == ' ' {
                continue
            }
            let p = NodeP1::new(x as i64, y as i64);
            if r == '.' {
                m.insert(p, Tile::new(p));
            }
            if y > innertop && y < innerbottom {
                if x == outerleft || x == innerright {
                    // read portal label to the left
                    let l1 = grid.get(y).unwrap().chars().nth(x-2).unwrap();
                    let l2 = grid.get(y).unwrap().chars().nth(x-1).unwrap();
                    let label = format!("{}{}", l1, l2);
                    connect_portals(&mut m, &mut labels, label, p);
                    continue
                }
                if x == outerright || x == innerleft {
                    // read portal label to the right
                    let l1 = grid.get(y).unwrap().chars().nth(x+1).unwrap();
                    let l2 = grid.get(y).unwrap().chars().nth(x+2).unwrap();
                    let label = format!("{}{}", l1, l2);
                    connect_portals(&mut m, &mut labels, label, p);
                    continue
                }
            }
            if x > innerleft && x < innerright {
                if y == outertop || y == innerbottom {
                    // read portal label above
                    let l1 = grid.get(y-2).unwrap().chars().nth(x).unwrap();
                    let l2 = grid.get(y-1).unwrap().chars().nth(x).unwrap();
                    let label = format!("{}{}", l1, l2);
                    connect_portals(&mut m, &mut labels, label, p);
                    continue
                }
                if y == outerbottom || y == innertop {
                    // read portal label below
                    let l1 = grid.get(y+1).unwrap().chars().nth(x).unwrap();
                    let l2 = grid.get(y+2).unwrap().chars().nth(x).unwrap();
                    let label = format!("{}{}", l1, l2);
                    connect_portals(&mut m, &mut labels, label, p);
                }
            }
        }
    }

    let dm = Donut{coords: m};

    let start = labels.get("AA").unwrap();
    let end = labels.get("ZZ").unwrap();

    println!("Part 1: {}", path::find_route(dm, *start, *end).unwrap().len() - 1);
}
