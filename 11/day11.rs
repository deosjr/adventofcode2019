use std::fs::File;
use std::io;
use std::io::Read;
use std::str::FromStr; use std::collections::VecDeque;
use std::collections::HashMap;

struct Program {
    mem: HashMap<i64, i64>,
    input: VecDeque<i64>,
    output: VecDeque<i64>,
    iptr: i64,
    relative_base: i64,
    halted: bool,
    error: bool
}

impl Program {
    fn new(program: &HashMap<i64, i64>) -> Program {
        Program{
            mem: program.clone(),
            input: VecDeque::new(),
            output: VecDeque::new(),
            iptr: 0,
            relative_base: 0,
            halted: false,
            error: false
        }
    }

    fn give_input(&mut self, i: i64) {
        self.input.push_back(i);
    }

    fn get_output(&mut self) -> Option<i64> {
        self.output.pop_front()
    }

    fn run(&mut self) {
        self.run_program();
        if self.error {
            panic!();
        }
    }

    fn value(&mut self, raw: i64, pos: u32) -> i64 {
        let mode = (raw / (10_i64.pow(pos-1))) % 10;
        match mode {
            0 => self.value_direct(self.value_direct(self.iptr+pos as i64)),
            1 => self.value_direct(self.iptr+pos as i64),
            2 => self.value_direct(self.value_direct(self.iptr+pos as i64)+self.relative_base),
            _ => panic!()
        }
    }

    fn value_direct(&self, pos: i64) -> i64 {
        if pos < 0 {
            panic!()
        }
        *(self.mem.get(&pos).or(Some(&0)).unwrap())
    }

    fn addr(&mut self, raw: i64, pos: u32) -> i64 {
        let mode = (raw / (10_i64.pow(pos-1))) % 10;
        match mode {
            0 => self.value_direct(self.iptr+pos as i64),
            2 => self.value_direct(self.iptr+pos as i64)+self.relative_base,
            _ => panic!()
        }
    }

    fn run_program(&mut self) {
        loop {
            let opcode_raw = self.value_direct(self.iptr);
            let opcode = opcode_raw % 100;
            let modes = opcode_raw / 100;
            match opcode {
                1 => self.add(modes), 
                2 => self.multiply(modes),
                3 => { if self.input.len() == 0 { return }; self.read(modes) }
                4 => self.write(modes),
                5 => self.jump_if_not_zero(modes), 
                6 => self.jump_if_zero(modes), 
                7 => self.less(modes), 
                8 => self.equal(modes), 
                9 => self.set_relative_base(modes),
                99 => { self.halted = true; return },
                _ => { self.error = true; return },
            }
        }
    }

    fn add(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        let z = self.addr(opcode_raw, 3); 
        self.mem.insert(z, x+y);
        self.iptr += 4;
    }

    fn multiply(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        let z = self.addr(opcode_raw, 3); 
        self.mem.insert(z, x*y);
        self.iptr += 4;
    }

    fn read(&mut self, opcode_raw: i64) {
        let x = self.addr(opcode_raw, 1); 
        match self.input.pop_front() {
            Some(i) => self.mem.insert(x, i),
            None => panic!()
        };
        self.iptr += 2;
    }

    fn write(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        self.output.push_back(x);
        self.iptr += 2;
    }

    fn jump_if_not_zero(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        self.iptr = if x != 0 { y } else { self.iptr+3 };
    }

    fn jump_if_zero(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        self.iptr = if x == 0 { y } else { self.iptr+3 };
    }

    fn less(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        let z = self.addr(opcode_raw, 3); 
        if x < y { self.mem.insert(z, 1) } else { self.mem.insert(z, 0) };
        self.iptr += 4;
    }

    fn equal(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        let z = self.addr(opcode_raw, 3); 
        if x == y { self.mem.insert(z, 1) } else { self.mem.insert(z, 0) };
        self.iptr += 4;
    }

    fn set_relative_base(&mut self, opcode_raw: i64) {
        self.relative_base += self.value(opcode_raw, 1);
        self.iptr += 2;
    }
}

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
    x: i32,
    y: i32
}

impl Coord {
    fn new(x: i32, y: i32) -> Coord {
        Coord{x:x, y:y}
    }

    fn turn_left(&mut self) {
        let (x,y) = (self.x, self.y);
        self.x = -y;
        self.y = x;
    }

    fn turn_right(&mut self) {
        let (x,y) = (self.x, self.y);
        self.x = y;
        self.y = -x;
    }
}

fn run_robot(brain: &HashMap<i64, i64>, panels: &mut HashMap<Coord, i64>) {
    let mut program = Program::new(&brain);
    let mut pos = Coord::new(0, 0);
    let mut direction = Coord::new(0, -1);
    while !program.halted {
        let camera = panels.get(&pos).or(Some(&0)).unwrap();
        program.give_input(*camera);
        program.run();
        let color = program.get_output().unwrap();
        panels.insert(pos, color);
        match program.get_output().unwrap() {
            0 => direction.turn_left(),
            1 => direction.turn_right(),
            _ => panic!()
        };
        pos.x += direction.x;
        pos.y += direction.y;
    }
}

fn main() {
    let input = parse("day11.input").unwrap();
    let mut panels = HashMap::new();
    run_robot(&input, &mut panels);
    println!("Part 1: {}", panels.len());
    let mut panels_single_white = HashMap::new();
    panels_single_white.insert(Coord::new(0,0), 1);
    run_robot(&input, &mut panels_single_white);
    let mut mincoords = Coord::new(std::i32::MAX, std::i32::MAX); 
    let mut maxcoords = Coord::new(std::i32::MIN, std::i32::MIN); 
    for k in panels_single_white.keys() {
        if k.x < mincoords.x {
            mincoords.x = k.x;
        }
        if k.y < mincoords.y {
            mincoords.y = k.y;
        }
        if k.x > maxcoords.x {
            maxcoords.x = k.x;
        }
        if k.y > maxcoords.y {
            maxcoords.y = k.y;
        }
    }
    println!("Part 2: ");
    for y in mincoords.y..maxcoords.y+1 {
        for x in mincoords.x..maxcoords.x+1 {
            let camera = panels_single_white.get(&Coord::new(x,y)).or(Some(&0)).unwrap();
            match camera {
                0 => print!(" "),
                1 => print!("\u{2588}"),
                _ => panic!()
            }
        }
        println!("");
    }
}
