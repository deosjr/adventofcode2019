use std::fs::File;
use std::io;
use std::io::Read;

fn parse(filename: &str) -> io::Result<Vec<usize>> {
    let mut file = File::open(filename)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let v: Vec<usize> = input.trim_end().chars().map(|c| c.to_digit(10).unwrap() as usize).collect();
    Ok(v)
}

fn part1(input: &Vec<usize>) -> usize {
    let mut ans = 0;
    let mut fewest = 150; // = 25 * 6
    for layer in input.chunks(150) {
        let zeroes = layer.iter().filter(|&x| *x == 0).count();
        let ones = layer.iter().filter(|&x| *x == 1).count();
        let twos = layer.iter().filter(|&x| *x == 2).count();
        if zeroes >= fewest {
           continue 
        }
        fewest = zeroes;
        ans = ones * twos;
    }
    ans
}

fn part2(input: &Vec<usize>) {
    let mut image: [usize; 150] = [2; 150];
    for layer in input.chunks(150) {
        for (i, c) in layer.iter().enumerate() {
            if image[i] != 2 {
                continue
            }
            if *c == 2 {
                continue
            }
            image[i] = *c;
        }
    }
    for row in image.chunks(25) {
        let prettyprint: Vec<char> = row.iter().map(|&x| if x==0 { ' ' } else { '\u{2588}' }).collect();
        let s: String = prettyprint.into_iter().collect();
        println!("{}", s);
    }
}

fn main() {
    let input = parse("day8.input").unwrap();
    println!("Part 1: {}", part1(&input));
    println!("Part 2:");
    part2(&input);
}
