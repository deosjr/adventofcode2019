use std::fs::File;
use std::io;
use std::io::Read;
use std::str::FromStr;
use std::collections::VecDeque;

struct Amplifier {
    mem: Vec<i32>,
    input: VecDeque<i32>,
    output: Vec<i32>,
    iptr: usize,
    halted: bool,
    error: bool
}

impl Amplifier {
    fn new(program: &Vec<i32>) -> Amplifier {
        let mut mem = vec![0; program.len()];
        mem.copy_from_slice(program);
        Amplifier{
            mem: mem,
            input: VecDeque::new(),
            output: Vec::new(),
            iptr: 0,
            halted: false,
            error: false
        }
    }

    fn give_input(&mut self, i: i32) {
        self.input.push_back(i);
    }

    fn get_output(&mut self) -> Option<i32> {
        self.output.pop()
    }

    fn run(&mut self) {
        self.run_program();
        if self.error {
            panic!();
        }
    }

    fn run_program(&mut self) {
        let mem = &mut self.mem;
        let input = &mut self.input;
        let output = &mut self.output;
        while self.iptr < mem.len() {
            let opcode_raw = mem[self.iptr];
            let opcode = opcode_raw % 100;
            match opcode {
                1 => {
                    let x = value(mem, opcode_raw, self.iptr, 1);
                    let y = value(mem, opcode_raw, self.iptr, 2);
                    let z = mem[self.iptr+3] as usize; 
                    mem[z] = x + y;
                    self.iptr += 4;
                },
                2 => {
                    let x = value(mem, opcode_raw, self.iptr, 1);
                    let y = value(mem, opcode_raw, self.iptr, 2);
                    let z = mem[self.iptr+3] as usize; 
                    mem[z] = x * y;
                    self.iptr += 4;
                },
                3 => {
                    let x = mem[self.iptr+1] as usize; 
                    match input.pop_front() {
                        Some(i) => mem[x] = i,
                        None => panic!() 
                    }
                    self.iptr += 2;
                },
                4 => {
                    output.push( value(mem, opcode_raw, self.iptr, 1) );
                    self.iptr += 2;
                    return
                },
                5 => {
                    let x = value(mem, opcode_raw, self.iptr, 1);
                    let y = value(mem, opcode_raw, self.iptr, 2);
                    self.iptr = if x != 0 { y as usize  } else { self.iptr+3 };
                },
                6 => {
                    let x = value(mem, opcode_raw, self.iptr, 1);
                    let y = value(mem, opcode_raw, self.iptr, 2);
                    self.iptr = if x == 0 { y as usize } else { self.iptr+3 };
                },
                7 => {
                    let x = value(mem, opcode_raw, self.iptr, 1);
                    let y = value(mem, opcode_raw, self.iptr, 2);
                    let z = mem[self.iptr+3] as usize; 
                    mem[z] = if x < y { 1 } else { 0 };
                    self.iptr += 4;
                },
                8 => {
                    let x = value(mem, opcode_raw, self.iptr, 1);
                    let y = value(mem, opcode_raw, self.iptr, 2);
                    let z = mem[self.iptr+3] as usize; 
                    mem[z] = if x == y { 1 } else { 0 };
                    self.iptr += 4;
                },
                99=> {
                    self.halted = true;
                    return
                },
                _ => { 
                    self.error = true;
                    return 
                },
            }
        }
    }
}

fn parse(filename: &str) -> io::Result<Vec<i32>> {
    let mut file = File::open(filename)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    Ok(str_to_vec(&input)) }

fn str_to_vec(input: &str) -> Vec<i32> {
    input.trim_end().split(',').map(|x| i32::from_str(&x).unwrap()).collect()
}

fn value(mem: &[i32], raw: i32, ip: usize, pos: usize) -> i32 {
    let rem = raw / (10 * (10_i32.pow(pos as u32)));
    if rem % 10 == 0 { mem[mem[ip+pos] as usize] } else { mem[ip+pos] } 
}

fn permutations(v: Vec<i32>) -> Vec<Vec<i32>> {
    let mut perms = Vec::new();
    for (i, x) in v.iter().enumerate() {
        let mut rem = v.clone();
        rem.remove(i);
        perms.append(&mut rec_perms(vec![vec![*x]], rem)); 
    }
    perms
}

fn rec_perms(sofar: Vec<Vec<i32>>, rem: Vec<i32>) -> Vec<Vec<i32>> {
    if rem.len() == 0 {
        return sofar 
    }
    let mut perms = Vec::new();
    for s in &sofar {
        for (i, x) in rem.iter().enumerate() {
            let mut news = s.clone();
            let mut newrem = rem.clone();
            newrem.remove(i);
            let mut newsofar = Vec::new();
            news.push(*x);
            newsofar.push(news);
            let mut rec = rec_perms(newsofar, newrem);
            perms.append(&mut rec);
        }
    }
    perms
}

fn run_amplifiers(program: &Vec<i32>, phases: Vec<i32>) -> i32 {
    let mut input = 0;
    for p in phases {
        let mut amp = Amplifier::new(program);
        amp.give_input(p);
        amp.give_input(input);
        amp.run();
        input = amp.get_output().unwrap(); 
    }
    input
}

fn find_max(program: &Vec<i32>, perms: Vec<Vec<i32>>, f: fn(&Vec<i32>,Vec<i32>)->i32) -> i32 {
    let mut max = 0;
    for p in perms {
        let out = f(program, p);
        if out > max {
            max = out
        }
    }
    max
}

fn part1(program: &Vec<i32>) -> i32 {
    find_max(program, permutations((0..5).collect()), run_amplifiers)
}

fn run_amplifiers_with_feedback(program: &Vec<i32>, phases: Vec<i32>) -> i32 {
    let mut amps = Vec::new();
    for i in 0..5 {
        let mut amp = Amplifier::new(program);
        amp.give_input(phases[i]);
        if i==0 {
            amp.give_input(0);
        }
        amps.push(amp); 
    }
    let mut last_output = 0;
    for i in (0..5).cycle() {
        amps[i].run();
        if amps[i].halted {
            if i==4 {
                break
            }
            continue
        }
        last_output = amps[i].get_output().unwrap();
        amps[(i+1)%5].give_input(last_output);
    }
    last_output 
}

fn part2(program: &Vec<i32>) -> i32 {
    find_max(program, permutations((5..10).collect()), run_amplifiers_with_feedback)
}

fn main() {
    let program = parse("day7.input").unwrap();
    let out1 = part1(&program);
    println!("Part 1: {}", out1); 
    let out2 = part2(&program);
    println!("Part 2: {}", out2); 
}

#[test]
fn test_part1() {
    let p1 = str_to_vec("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0");
    assert_eq!(part1(&p1), 43210);
    let p2 = str_to_vec("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0");
    assert_eq!(part1(&p2), 54321);
    let p3 = str_to_vec("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0");
    assert_eq!(part1(&p3), 65210);
}

#[test]
fn test_run_amplifiers() {
    let p1 = str_to_vec("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0");
    assert_eq!(run_amplifiers(&p1, vec![4,3,2,1,0]), 43210);
}

#[test]
fn test_part2() {
    let p1 = str_to_vec("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5");
    assert_eq!(part2(&p1), 139629729);
    let p2 = str_to_vec("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10");
    assert_eq!(part2(&p2), 18216);
}

#[test]
fn test_run_amplifiers_with_feedback() {
    let p1 = str_to_vec("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5");
    assert_eq!(run_amplifiers_with_feedback(&p1, vec![9,8,7,6,5]), 139629729);
}
