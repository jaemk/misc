
pub fn solve1(input: &str) -> u32 {
    let a = input.trim().chars().collect::<Vec<_>>();
    let mut b = a.clone();
    let first_char = b.remove(0);
    b.push(first_char);
    a.iter().zip(b.iter()).fold(0, |acc, (c, next)| {
        if c != next { acc }
        else {
            acc + c.to_digit(10).expect("invalid digit")
        }
    })
}


pub fn solve1_nocopy(input: &str) -> u32 {
    let chars = input.trim().chars().collect::<Vec<_>>();
    let len = chars.len();
    let mut sum = 0;
    for i in 0..len {
        let next = if i == len - 1 {
            chars[0]
        } else {
            chars[i + 1]
        };
        if chars[i] == next {
            sum += next.to_digit(10).expect("invalid digit");
        }
    }
    sum
}


pub fn solve1_bytes(input: &str) -> u32 {
    let a = input.trim().as_bytes().to_vec();
    let mut b = a.clone();
    let first_char = b.remove(0);
    b.push(first_char);
    a.iter().zip(b.iter()).fold(0, |acc, (c, next)| {
        if c != next { acc }
        else {
            acc + (*c as char).to_digit(10).expect("invalid digit")
        }
    })
}


pub fn solve1_bytes_nocopy(input: &str) -> u32 {
    let bytes = input.trim().as_bytes();
    let len = bytes.len();
    let mut sum = 0;
    for i in 0..len {
        let next = if i == len - 1 {
            bytes[0]
        } else {
            bytes[i + 1]
        };
        if bytes[i] == next {
            sum += (next as char).to_digit(10).expect("invalid digit");
        }
    }
    sum
}



pub fn solve2(input: &str) -> u32 {
    let chars = input.trim().chars().collect::<Vec<_>>();
    let len = chars.len();
    let jump = len / 2;
    chars.iter().enumerate().fold(0, |acc, (i, c)| {
        let mut next_idx = i + jump;
        while next_idx >= len { next_idx -= len; }
        if *c == chars[next_idx] {
            acc + c.to_digit(10).expect("invalid digit")
        } else {
            acc
        }
    })
}


pub fn solve2_modulo(input: &str) -> u32 {
    let chars = input.trim().chars().collect::<Vec<_>>();
    let len = chars.len();
    let jump = len / 2;
    chars.iter().enumerate().fold(0, |acc, (i, c)| {
        let next_idx = i + jump;
        let next_idx = if next_idx > len - 1 { next_idx % len } else { next_idx };
        if *c == chars[next_idx] {
            acc + c.to_digit(10).expect("invalid digit")
        } else {
            acc
        }
    })
}


pub fn solve2_bytes(input: &str) -> u32 {
    let bytes = input.trim().as_bytes();
    let len = bytes.len();
    let jump = len / 2;
    bytes.iter().enumerate().fold(0, |acc, (i, c)| {
        let mut next_idx = i + jump;
        while next_idx >= len { next_idx -= len; }
        if *c == bytes[next_idx] {
            acc + (*c as char).to_digit(10).expect("invalid digit")
        } else {
            acc
        }
    })
}


pub fn solve2_bytes_modulo(input: &str) -> u32 {
    let bytes = input.trim().as_bytes();
    let len = bytes.len();
    let jump = len / 2;
    bytes.iter().enumerate().fold(0, |acc, (i, c)| {
        let next_idx = i + jump;
        let next_idx = if next_idx > len - 1 { next_idx % len } else { next_idx };
        if *c == bytes[next_idx] {
            acc + (*c as char).to_digit(10).expect("invalid digit")
        } else {
            acc
        }
    })
}

