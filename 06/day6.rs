use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::collections::HashMap;

fn read_file(filename: &str) -> io::Result<io::BufReader<File>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    Ok(reader) 
}

fn parse(reader: io::BufReader<File>) -> (HashMap<String, String>, HashMap<String, Vec<String>>) {
    let mut c2p: HashMap<String, String> = HashMap::new();
    let mut p2c: HashMap<String, Vec<String>> = HashMap::new();
    for line in reader.lines() {
        let line = line.as_ref().unwrap();
        let s: Vec<String> = line.split(")").map(str::to_owned).collect();
        let from = s[0].clone();
        let to = s[1].clone();
        let from2 = s[0].clone();
        let to2 = s[1].clone();
        if p2c.contains_key(&from) {
            let v = p2c.get_mut(&from).unwrap();
            v.push(to);
        } else {
            p2c.insert(from, vec![to]);
        }
        c2p.insert(to2, from2);
    }
    (c2p, p2c)
}

fn part1(p2c: &HashMap<String, Vec<String>>) -> i32 {
    count_orbits(p2c, "COM".to_string(), 0)
}

fn count_orbits(p2c: &HashMap<String, Vec<String>>, root: String, depth: i32) -> i32 {
    match p2c.get(&root) {
        Some(children) => {
            let mut sum = depth;

            for child in children {
                sum += count_orbits(p2c, child.to_string(), depth+1);
            }
            sum
        },
        None => depth
    }
}

fn part2(c2p: &HashMap<String, String>, p2c: &mut HashMap<String, Vec<String>>) -> i32 {
    let com = "COM".to_string();
    find_and_prune(c2p, p2c, com);
    count_path(c2p, p2c, "YOU".to_string(), "SAN".to_string())
}

fn find_and_prune(c2p: &HashMap<String, String>, p2c: &mut HashMap<String, Vec<String>>, root: String) {
    if root == "YOU" || root == "SAN" {
        return
    }
    match p2c.get(&root) {
        Some(cs) => {
            for child in cs.clone() {
                find_and_prune(c2p, p2c, child.to_string());
            }
        },
        None => prune(c2p, p2c, root)
    }
}

fn prune(c2p: &HashMap<String, String>, p2c: &mut HashMap<String, Vec<String>>, leaf: String) {
    let mut s = leaf;
    loop {
        let parent = c2p.get(&s).unwrap();
        let mut newchildren = Vec::new();
        let children = p2c.get(parent).unwrap();
        if children.len() > 1 {
            for child in children {
                if *child != s {
                    newchildren.push(child.to_string());
                }
            }
            p2c.insert(parent.to_string(), newchildren);
            return
        }
        p2c.remove(parent);
        s = parent.to_string();
    }
}

fn count_path(c2p: &HashMap<String, String>, p2c: &mut HashMap<String, Vec<String>>, from: String, to:String) -> i32 {
    let (next, sum) = count_up(c2p, p2c, from);
    sum + count_down(p2c, next, to)
}

fn count_up(c2p: &HashMap<String, String>, p2c: &mut HashMap<String, Vec<String>>, from: String) -> (String, i32) {
    let parent = c2p.get(&from).unwrap();
    let children = p2c.get(parent).unwrap();
    if children.len() > 1 {
        for child in children {
            if *child == from {
                continue
            }
            return (child.to_string(), 0)
        }
    }
    let (next, sum) = count_up(c2p, p2c, parent.to_string());
    (next.to_string(), sum+1)
}

fn count_down(p2c: &mut HashMap<String, Vec<String>>, next: String, to:String) -> i32 {
    let children = p2c.get(&next).unwrap().to_vec();
    let child = &children[0];
    if *child == to {
        return 1 
    }
    count_down(p2c, child.to_string(), to) + 1
}

fn main() {
    let reader = read_file("day6.input").unwrap();
    let (child_to_parent, mut parent_to_children) = parse(reader);
    println!("Part 1: {}", part1(&parent_to_children));
    println!("Part 2: {}", part2(&child_to_parent, &mut parent_to_children));
}
