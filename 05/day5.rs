use std::fs::File;
use std::io;
use std::io::Read;
use std::str::FromStr;

fn parse(filename: &str) -> io::Result<Vec<i32>> {
    let mut file = File::open(filename)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    Ok(str_to_vec(&input))
}

fn str_to_vec(input: &str) -> Vec<i32> {
    input.trim_end().split(',').map(|x| i32::from_str(&x).unwrap()).collect()
}

fn value(mem: &[i32], raw: i32, ip: usize, pos: usize) -> i32 {
    let rem = raw / (10 * (10_i32.pow(pos as u32)));
    if rem % 10 == 0 { mem[mem[ip+pos] as usize] } else { mem[ip+pos] } 
}

fn run_program(program: &Vec<i32>, input: i32) -> Option<i32>{
    let mut mem = vec![0; program.len()];
    mem.copy_from_slice(program);
    let mut last_output = -1;
    let mut ip = 0;
    while ip < mem.len() {
        let opcode_raw = mem[ip];
        let opcode = opcode_raw % 100;
        match opcode {
            1 => {
                let x = value(&mem, opcode_raw, ip, 1);
                let y = value(&mem, opcode_raw, ip, 2);
                let z = mem[ip+3] as usize; 
                mem[z] = x + y;
                ip += 4;
            },
            2 => {
                let x = value(&mem, opcode_raw, ip, 1);
                let y = value(&mem, opcode_raw, ip, 2);
                let z = mem[ip+3] as usize; 
                mem[z] = x * y;
                ip += 4;
            },
            3 => {
                let x = mem[ip+1] as usize; 
                mem[x] = input;
                ip += 2;
            },
            4 => {
                let x = value(&mem, opcode_raw, ip, 1);
                last_output = x;
                ip += 2;
            },
            5 => {
                let x = value(&mem, opcode_raw, ip, 1);
                let y = value(&mem, opcode_raw, ip, 2);
                ip = if x != 0 { y as usize  } else { ip+3 };
            },
            6 => {
                let x = value(&mem, opcode_raw, ip, 1);
                let y = value(&mem, opcode_raw, ip, 2);
                ip = if x == 0 { y as usize } else { ip+3 };
            },
            7 => {
                let x = value(&mem, opcode_raw, ip, 1);
                let y = value(&mem, opcode_raw, ip, 2);
                let z = mem[ip+3] as usize; 
                mem[z] = if x < y { 1 } else { 0 };
                ip += 4;
            },
            8 => {
                let x = value(&mem, opcode_raw, ip, 1);
                let y = value(&mem, opcode_raw, ip, 2);
                let z = mem[ip+3] as usize; 
                mem[z] = if x == y { 1 } else { 0 };
                ip += 4;
            },
            99=> {
                break
            },
            _ => { 
                return None
            },
        }
    }
    Some(last_output)
}

fn main() {
    let program = parse("day5.input").unwrap();
    println!("Part 1: {}", run_program(&program, 1).unwrap()); 
    println!("Part 2: {}", run_program(&program, 5).unwrap()); 
}

#[test]
fn test_comparisons() {
    let p1 = str_to_vec("3,9,8,9,10,9,4,9,99,-1,8");
    assert_eq!(run_program(&p1, 8).unwrap(), 1);
    assert_eq!(run_program(&p1, 9).unwrap(), 0);
    let p2 = str_to_vec("3,9,7,9,10,9,4,9,99,-1,8");
    assert_eq!(run_program(&p2, 4).unwrap(), 1);
    assert_eq!(run_program(&p2, 9).unwrap(), 0);
    let p3 = str_to_vec("3,3,1108,-1,8,3,4,3,99");
    assert_eq!(run_program(&p3, 8).unwrap(), 1);
    assert_eq!(run_program(&p3, 1).unwrap(), 0);
    let p4 = str_to_vec("3,3,1107,-1,8,3,4,3,99");
    assert_eq!(run_program(&p4, 3).unwrap(), 1);
    assert_eq!(run_program(&p4, 11).unwrap(), 0);
}

#[test]
fn test_jump() {
    let p1 = str_to_vec("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9");
    assert_eq!(run_program(&p1, 0).unwrap(), 0);
    assert_eq!(run_program(&p1, 8).unwrap(), 1);
    let p2 = str_to_vec("3,3,1105,-1,9,1101,0,0,12,4,12,99,1");
    assert_eq!(run_program(&p2, 0).unwrap(), 0);
    assert_eq!(run_program(&p2, 8).unwrap(), 1);
}
