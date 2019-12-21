use std::collections::VecDeque;
use std::collections::HashMap;

pub struct Program {
    mem: HashMap<i64, i64>,
    input: VecDeque<i64>,
    output: VecDeque<i64>,
    iptr: i64,
    relative_base: i64,
    halted: bool,
    error: bool
}

impl Program {
    pub fn new(program: &HashMap<i64, i64>) -> Program {
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

    pub fn give_input(&mut self, i: i64) {
        self.input.push_back(i);
    }

    pub fn give_ascii_input(&mut self, s: &str) {
        for c in s.chars() {
            self.input.push_back(c as i64);
        }
        self.input.push_back(10);
    }

    pub fn get_output(&mut self) -> Option<i64> {
        self.output.pop_front()
    }

    pub fn get_all_output(&mut self) -> Vec<i64> {
        let v = self.output.iter().map(|&x| x).collect::<Vec<i64>>();
        self.output.clear();
        v
    }

    pub fn run(&mut self) {
        self.run_program();
        if self.error {
            panic!();
        }
    }

    pub fn halted(&mut self) -> bool {
        self.halted
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
