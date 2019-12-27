use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::collections::HashMap;
use std::collections::BinaryHeap;

fn parse(filename: &str) -> Vec<String> {
    let file = File::open(filename).expect("");
    let reader = io::BufReader::new(file);
    reader.lines().map(|l| l.unwrap()).collect::<Vec<String>>() }

#[derive(Debug,Ord,PartialOrd,Eq,PartialEq)]
struct PQItem {
    fscore: i64,
    node: Node
}

impl PQItem {
    fn new(node: Node, fscore: i64) -> PQItem {
        PQItem{
            fscore: fscore,
            node:   node,
        }
    }
}

#[derive(Debug,Ord,PartialOrd,Eq,PartialEq,Hash,Copy,Clone)]
struct Node {
    x: i64,
    y: i64
}

impl Node {
    fn new(x: i64, y: i64) -> Node {
        Node{x: x, y: y}
    }
}

trait Map {
    fn neighbours(&self, n: Node) -> Vec<Node>;
    fn g(&self, n: Node, neighbour: Node) -> i64;
    fn h(&self, n: Node, goal: Node) -> i64;
}

struct Donut {
    coords: HashMap<Node,Tile>
}

impl Map for Donut {
    fn neighbours(&self, n: Node) -> Vec<Node> {
        let mut neighbours: Vec<Node> = vec![];
        let npoints = [ Node::new(n.x + 1, n.y),
                        Node::new(n.x - 1, n.y),
                        Node::new(n.x, n.y + 1),
                        Node::new(n.x, n.y - 1) ];
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

    fn g(&self, _n: Node, _neighbour: Node) -> i64 { 1 }

    fn h(&self, n: Node, goal: Node) -> i64 {
        let dx = goal.x - n.x;
        let dy = goal.y - n.y;
        dx.abs() + dy.abs()
    }
}

#[derive(Clone)]
struct Tile {
    point: Node,
    warp:  Node
}

impl Tile {
    fn new(p: Node) -> Tile {
        Tile{point: p, warp: Node::new(0,0)}
    }
}

fn find_route<T: Map>(map: T, start: Node, goal: Node) -> Option<Vec<Node>>{
    let mut open_set: HashMap<Node,bool> = HashMap::new();
    open_set.insert(start, true);

    let mut came_from: HashMap<Node,Node> = HashMap::new();

    let mut g_score: HashMap<Node,i64> = HashMap::new();
    g_score.insert(start, 0);

    let mut f_score: HashMap<Node,i64> = HashMap::new();
    let start_h = map.h(start, goal);
    f_score.insert(start, start_h);

    let mut pq = BinaryHeap::new();
    pq.push(PQItem::new(start, start_h));

    let mut goal_score = std::i64::MAX;

    while !pq.is_empty() {
        let item = pq.pop().unwrap();
        if item.fscore == std::i64::MAX {
            break
        }
        let current = item.node;
        let current_gscore = *(g_score.get(&current).unwrap());
        if current == goal {
            goal_score = current_gscore;
        }
        open_set.remove(&current);

        for n in map.neighbours(current) {
            let tentative_gscore = current_gscore + map.g(current, n);
            let f = tentative_gscore + map.h(n, goal);

            let interesting_f = match f_score.get(&n) {
                None => true,
                Some(&v) => f < v
            };

            if !open_set.contains_key(&n) && interesting_f && f < goal_score {
                open_set.insert(n, true);
                pq.push(PQItem::new(n, f));
            } else if tentative_gscore >= *(g_score.get(&n).unwrap()) {
                continue
            }
            came_from.insert(n, current);
            g_score.insert(n, tentative_gscore);
            f_score.insert(n, f);
        }
    }
    if goal_score == std::i64::MAX {
        None
    } else {
        Some(reconstruct_path(came_from, goal))
    }
}

fn reconstruct_path(came_from: HashMap<Node,Node>, mut current: Node) -> Vec<Node> {
    let mut path = vec![current];
    loop {
        match came_from.get(&current) {
            None => break,
            Some(&prev) => {
                current = prev;
                path.push(current);
            }
        }
    }
    path
}

fn connect_portals(m: &mut HashMap<Node,Tile>, labels: &mut HashMap<String,Node>, label: String, p1: Node) {
    match labels.get(&label) {
        Some(&p2) => {
            let t1 = m.get(&p1).unwrap().clone();
            let t2 = m.get(&p2).unwrap().clone();
            m.insert(p1, Tile{point: t1.point, warp: p2});
            m.insert(p2, Tile{point: t2.point, warp: p1});
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

    let mut m: HashMap<Node,Tile> = HashMap::new();
    let mut labels: HashMap<String,Node> = HashMap::new();
    for (y, line) in grid.iter().enumerate() {
        for (x, r) in line.chars().enumerate() {
            if r == '#' || r == ' ' {
                continue
            }
            let p = Node::new(x as i64, y as i64);
            if r == '.' {
                m.insert(p, Tile::new(p));
            }
            if y > innertop && y < innerbottom && (x == outerleft || x == innerright) {
                // read portal label to the left
                let l1 = grid.get(y).unwrap().chars().nth(x-2).unwrap();
                let l2 = grid.get(y).unwrap().chars().nth(x-1).unwrap();
                let label = format!("{}{}", l1, l2);
                connect_portals(&mut m, &mut labels, label, p);
            }
            if x > innerleft && x < innerright && (y == outertop || y == innerbottom) {
                // read portal label above
                let l1 = grid.get(y-2).unwrap().chars().nth(x).unwrap();
                let l2 = grid.get(y-1).unwrap().chars().nth(x).unwrap();
                let label = format!("{}{}", l1, l2);
                connect_portals(&mut m, &mut labels, label, p);
            }
            if y > innertop && y < innerbottom && (x == outerright || x == innerleft) {
                // read portal label to the right
                let l1 = grid.get(y).unwrap().chars().nth(x+1).unwrap();
                let l2 = grid.get(y).unwrap().chars().nth(x+2).unwrap();
                let label = format!("{}{}", l1, l2);
                connect_portals(&mut m, &mut labels, label, p);
            }
            if x > innerleft && x < innerright && (y == outerbottom || y == innertop) {
                // read portal label below
                let l1 = grid.get(y+1).unwrap().chars().nth(x).unwrap();
                let l2 = grid.get(y+2).unwrap().chars().nth(x).unwrap();
                let label = format!("{}{}", l1, l2);
                connect_portals(&mut m, &mut labels, label, p);
            }
        }
    }

    let dm = Donut{coords: m};

    let start = labels.get("AA").unwrap();
    let end = labels.get("ZZ").unwrap();

    println!("Part 1: {}", find_route(dm, *start, *end).unwrap().len() - 1);
}
