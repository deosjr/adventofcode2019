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

    fn run_to_halt(&mut self) {
        while !self.halted {
            self.run();
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
                3 => self.read(modes),
                4 => { self.write(modes); return },
                5 => self.not_zero(modes), 
                6 => self.is_zero(modes), 
                7 => self.jump_if_less(modes), 
                8 => self.jump_if_equal(modes), 
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

    fn not_zero(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        self.iptr = if x != 0 { y } else { self.iptr+3 };
    }

    fn is_zero(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        self.iptr = if x == 0 { y } else { self.iptr+3 };
    }

    fn jump_if_less(&mut self, opcode_raw: i64) {
        let x = self.value(opcode_raw, 1);
        let y = self.value(opcode_raw, 2);
        let z = self.addr(opcode_raw, 3); 
        if x < y { self.mem.insert(z, 1) } else { self.mem.insert(z, 0) };
        self.iptr += 4;
    }

    fn jump_if_equal(&mut self, opcode_raw: i64) {
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

fn main() {
    let input = parse("day9.input").unwrap();
    let mut program = Program::new(&input);
    program.give_input(1);
    program.run_to_halt();
    println!("Part 1: {}", program.get_output().unwrap());
    program = Program::new(&input);
    program.give_input(2);
    program.run_to_halt();
    println!("Part 2: {}", program.get_output().unwrap());
}

#[test]
fn test_quine_one_instr_per() {
    let p = str_to_map("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99");
    let mut program = Program::new(p);
    program.set_relative_base(1);
    assert_eq!(program.relative_base, 1);
    program.write(2);
    assert_eq!(program.get_output(), Some(109));
}

#[test]
fn test_quine() {
    let p = str_to_map("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99");
    let mut program = Program::new(p);
    program.run_to_halt();
    assert_eq!(program.output, vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]);
}

#[test]
fn test_16_digit() {
    let p = str_to_map("1102,34915192,34915192,7,4,7,99,0");
    let mut program = Program::new(p);
    program.run();
    assert_eq!(program.get_output(), Some(1219070632396864));
}

#[test]
fn test_large_number() {
    let p = str_to_map("104,1125899906842624,99");
    let mut program = Program::new(p);
    program.run();
    assert_eq!(program.get_output(), Some(1125899906842624));
}
