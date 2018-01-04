
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


pub fn solve1_indexchain(input: &str) -> u32 {
    let chars = input.trim().chars().collect::<Vec<_>>();
    let mut sum = 0;
    for (a, b) in (1..chars.len()).chain(0..1).enumerate() {
        let a = chars[a];
        if a == chars[b] {
            sum += a.to_digit(10).expect("invalid digit");
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


pub fn solve1_bytes_nocast(input: &str) -> u32 {
    let a = input.trim().as_bytes().to_vec();
    let mut b = a.clone();
    let first_char = b.remove(0);
    b.push(first_char);
    a.iter().zip(b.iter()).fold(0, |acc, (c, next)| {
        if c != next { acc }
        else {
            acc + (*c - b'0') as u16
        }
    }) as u32
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


pub fn solve1_bytes_nocopy_iterator(input: &str) -> u32 {
    let mut bytes = input.trim().bytes();
    let mut sum = 0;
    let first = bytes.next().unwrap();
    let mut prev = bytes.next().unwrap();
    if first == prev { sum += (first as char).to_digit(10).unwrap(); }
    for b in bytes {
        if b == prev { sum += (b as char).to_digit(10).unwrap(); }
        prev = b;
    }
    if first == prev { sum += (first as char).to_digit(10).unwrap(); }
    sum
}


pub fn solve1_bytes_nocopy_iterator_nocharcast(input: &str) -> u32 {
    let mut bytes = input.trim().bytes();
    let mut sum = 0;
    let first = bytes.next().unwrap();
    let mut prev = bytes.next().unwrap();
    if first == prev { sum += (first - b'0') as u16; }
    for b in bytes {
        if b == prev { sum += (b - b'0') as u16; }
        prev = b;
    }
    if first == prev { sum += (first - b'0') as u16; }
    sum as u32
}


pub fn solve1_bytes_indexchain(input: &str) -> u32 {
    let bytes = input.trim().as_bytes();
    let mut sum = 0;
    for (a, b) in (1..bytes.len()).chain(0..1).enumerate() {
        let a = bytes[a];
        if a == bytes[b] {
            sum += (a as char).to_digit(10).expect("invalid digit");
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

