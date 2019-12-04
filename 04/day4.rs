fn test1(n: i32) -> bool {
    let mut prev = n % 10;
    let mut rem = n / 10;
    let mut found = false;
    for _ in 0..5 {
        let next = rem % 10;
        if prev < next {
            return false 
        }
        if prev == next {
            found = true
        }
        prev = next;
        rem = rem / 10;
    }
    found
}

fn part1() -> i32 {
    let mut num = 0;
    for i in 307237..769058 {
        if test1(i) {
            num += 1;
        }
    }
    num
}

fn test2(n: i32) -> bool {
    let mut prev = n % 10;
    let mut rem = n / 10;
    let mut found = false;
    let mut same = 0;
    for _ in 0..5 {
        let next = rem % 10;
        if prev < next {
            return false 
        }
        if prev == next {
            same += 1;
        } else {
            if same == 1 {
                found = true;
            }
            same = 0;
        }
        prev = next;
        rem = rem / 10;
    }
    (same==1)||found
}

fn part2() -> i32 {
    let mut num = 0;
    for i in 307237..769058 { if test2(i) {
            num += 1;
        }
    }
    num 
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}

#[test]
fn test_part1() {
    assert!(test1(111111));
    assert!(!test1(223450));
    assert!(!test1(123789));
}

#[test]
fn test_part2() {
    assert!(test2(112233));
    assert!(!test2(123444));
    assert!(test2(111122));
    assert!(test2(112345));
}
