use std::fs::File;
use std::io;
use std::io::Read;

fn parse(filename: &str) -> io::Result<Vec<i32>> {
    let mut file = File::open(filename)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let v = input.trim_end().chars().map(|c| c.to_digit(10).unwrap() as i32).collect::<Vec<_>>();
    Ok(v)
}

fn f(index: usize, pos: usize) -> i32 {
    let i = index % ( 4 * pos ); 
    if i > 3 * pos - 1 {
        -1
    } else if i >= pos && i < 2 * pos {
        1
    } else {
        0
    }
}

fn phase(signal: Vec<i32>) -> Vec<i32> {
    let mut output = signal.clone();
    for i in 0..signal.len() {
        let mut sum: i32 = 0;
        let mut j = i;
        while j < signal.len() {
            let m = f(j+1, i+1); 
            if m == 0 {
                j += i+1;
                continue
            }
            let end = if j+i+1 < signal.len() { j+i+1 } else { signal.len() };
            let numsum: i32 = signal[j..end].iter().sum();
            if m == 1 {
                sum += numsum;
            } else if m == -1 {
                sum -= numsum;
            }
            j += i+1;
        }
        output[i] = sum.abs() % 10;
    }
    output
}

fn main() {
    let mut input = parse("day16.input").unwrap();
    for _ in 0..100 {
        input = phase(input);
    }
    print!("Part 1: ");
    for x in input[0..8].iter() {
        print!("{}", x);
    }
    println!();
}

#[test]
fn test_f3() {
    let v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]; 
    let fs = v.iter().map(|&x| f(x, 3)).collect::<Vec<i32>>();
    assert_eq!(fs, [0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1, 0]);
}

#[test]
fn test_f4() {
    let v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]; 
    let fs = v.iter().map(|&x| f(x, 4)).collect::<Vec<i32>>();
    assert_eq!(fs, [0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, -1]);
}

#[test]
fn test_phase() {
    let input = vec![1, 2, 3, 4, 5, 6, 7, 8]; 
    let p = phase(input);
    assert_eq!(p, [4, 8, 2, 2, 6, 1, 5, 8]);
}

#[test]
fn test_longer_phase() {
    let mut input = vec![8,0,8,7,1,2,2,4,5,8,5,9,1,4,5,4,6,6,1,9,0,8,3,2,1,8,6,4,5,5,9,5]; 
    for _ in 0..100 {
        input = phase(input);
    }
    assert_eq!(input[0..8], [2,4,1,7,6,1,7,6]);
}
