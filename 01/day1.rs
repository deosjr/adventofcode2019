use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

fn read_file(filename: &str) -> io::Result<io::BufReader<File>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    Ok(reader) 
}

fn main() {
    let reader = read_file("day1.input").unwrap();
    let mut sum1 = 0;
    let mut sum2 = 0;
    for line in reader.lines() {
        let line = line.unwrap();
        let x = i32::from_str(&line).unwrap();
        let mut y = (x/3)-2;
        sum1 += y;

        while y > 0 {
            sum2 += y;
            y = (y/3)-2;
        }
    }
    println!("Part 1: {}", sum1);
    println!("Part 2: {}", sum2);
}
