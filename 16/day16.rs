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

fn phase2(signal: Vec<i32>) -> Vec<i32> {
    let mut newphase = signal.clone();
    let mut sum = 0;
    for i in (0..signal.len()).rev() {
        sum = (sum + signal[i]) % 10;
        newphase[i] = sum;
    }
    newphase
}

fn main() {
    let input = parse("day16.input").unwrap();
    let mut part1 = input.clone();
    for _ in 0..100 {
        part1 = phase(part1);
    }
    println!("Part 1: {}", part1.iter().take(8).map(|&d| d.to_string()).collect::<String>());

    let offset = input.iter().take(7).map(|&d| d.to_string()).collect::<String>().parse::<i32>().unwrap();
    let mut part2 = input.clone();
    for _ in 1..10000 {
        part2.extend(input.clone());
    }
    part2 = part2.iter().skip(offset as usize).map(|&x| x).collect();
    for _ in 0..100 {
        part2 = phase2(part2);
    }
    println!("Part 2: {}", part2.iter().take(8).map(|&d| d.to_string()).collect::<String>());
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

#[test]
fn test_part2() {
    let input = vec![0,3,0,3,6,7,3,2,5,7,7,2,1,2,9,4,4,0,6,3,4,9,1,5,6,5,4,7,4,6,6,4]; 
    let mut part2 = input.clone();
    for _ in 1..10000 {
        part2.extend(input.clone());
    }
    part2 = part2.iter().skip(303673).map(|&x| x).collect();
    for _ in 0..100 {
        part2 = phase2(part2);
    }
    assert_eq!(part2[0..8], [8,4,4,6,2,0,2,6]);
}
