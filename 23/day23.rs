use std::fs::File;
use std::io;
use std::io::Read;
use std::str::FromStr;
use std::collections::HashMap;
use std::thread;
use std::sync::mpsc;
use std::time::Duration;

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

#[derive(Debug)]
struct Coord {
    x: i64, y: i64
}

impl Coord {
    fn new(x: i64, y: i64) -> Coord {
        Coord{x:x, y:y}
    }
}

#[derive(Debug)]
struct Message {
    address: usize,
    coord: Coord
}

impl Message {
    fn new(a: usize, x: i64, y: i64) -> Message {
        Message{address: a, coord:Coord::new(x, y)}
    }
}

fn part1(mut program: intcode::Program, tx: mpsc::Sender<Message>, rx: mpsc::Receiver<Coord>) {
    loop {
        program.run();
        let out = program.get_all_output();
        for c in out.chunks(3) {
            let (a, x, y) = (c[0], c[1], c[2]);
            tx.send(Message::new(a as usize, x, y));
        }
        if program.halted() {
            break
        }
        match rx.try_recv() {
            Ok(c)  => {program.give_input(c.x); program.give_input(c.y)}, 
            Err(_) => program.give_input(-1),
        }
    }
}

fn main() {
    let input = parse("day23.input").unwrap();
    let mut threads = Vec::new();
    let mut sends = Vec::new();
    let (main_tx, main_rx) = mpsc::channel();

    for i in 0..50 {
        let mut p = intcode::Program::new(&input);
        p.give_input(i);
        let (stx, srx) = mpsc::channel();
        sends.push(stx);
        let mtx = main_tx.clone();
        let t = thread::spawn(move || part1(p, mtx, srx));
        threads.push(t);
    }

    let mut nat = Coord::new(-1, -1);
    let mut lasty = -1;

    loop {
        let recv = main_rx.recv_timeout(Duration::from_millis(100));
        match recv {
            Ok(message) => {
                if message.address == 255 {
                    if nat.x == -1 && nat.y == -1 {
                        println!("Part 1: {}", message.coord.y);
                    }
                    nat = message.coord;
                    continue
                }
                sends[message.address].send(message.coord);
            },
            Err(_) => {
                if nat.y == lasty {
                    println!("Part 2: {}", lasty);
                    break
                }
                sends[0].send(Coord::new(nat.x, nat.y));
                lasty = nat.y;
            },
        }
    }
}
