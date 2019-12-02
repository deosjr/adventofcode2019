use std::fs::File;
use std::io;
use std::io::Read;
use std::str::FromStr;

fn parse(filename: &str) -> io::Result<Vec<usize>> {
    let mut file = File::open(filename)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let v: Vec<usize> = input.trim_end().split(',').map(|x| usize::from_str(&x).unwrap()).collect();
    Ok(v)
}

fn run_program(input: &Vec<usize>, noun: usize, verb: usize) -> Option<usize> {
    let mut mem = vec![0; input.len()];
    mem.copy_from_slice(input);
    mem[1] = noun;
    mem[2] = verb;
    for ip in (0..mem.len()).step_by(4) {
        if ip+3 > mem.len()-1 {
            break
        }
        let opcode = mem[ip];
        let x = mem[ip+1];
        let y = mem[ip+2];
        let z = mem[ip+3];
        match opcode {
            1 => mem[z] = mem[x] + mem[y],
            2 => mem[z] = mem[x] * mem[y],
            99=> break,
            _ => return None,
        }
    }
    Some(mem[0])
}

fn main() {
    let input = parse("day2.input").unwrap();
    let out1 = run_program(&input, 12, 2).unwrap();
    println!("Part 1: {}", out1);
    for n in 0..100 {
        for v in 0..100 {
            let ans = run_program(&input, n, v).unwrap();
            if ans != 19690720 {
                continue
            }
            let out2 = 100 * n + v;
            println!("Part 2: {}", out2);
        }
    }
}
