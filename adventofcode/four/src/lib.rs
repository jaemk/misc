use std::collections::HashMap;


pub enum Part {
    One,
    Two,
}

/// Fill a given buffer to its capacity from a vec of buckets(vecs)
fn fill_to_cap(buff: &mut Vec<char>, buckets: Vec<(&&i32, &Vec<&char>)>) {
    for (_, group) in buckets.into_iter() {
        let mut g = group.clone();
        g.sort();
        for c in g.iter() {
            buff.push(**c);
            if buff.len() == buff.capacity() { return; }
        }
    }
}

/// Return a vec of valid encrypted room strings and their sector-ID
/// Valid strings have a checksum equal to the first 5 highest frequency
/// chars ordered by frequency and then alphabetically.
fn filter_valid(content: &String) -> Vec<(String, u32)> {
    content.trim().split('\n').fold(vec![], |mut acc, line| {
        let mut parts_iter = line.split('-').rev();
        let id_check = parts_iter.next().unwrap();
        let mut counts = HashMap::new();
        for part in parts_iter {
            for c in part.chars() {
                *counts.entry(c).or_insert(0) += 1;
            }
        }
        let mut buckets = HashMap::new();
        for (c, count) in counts.iter() {
            let bucket = buckets.entry(count).or_insert(vec![]);
            bucket.push(c);
        }
        let mut buckets = buckets.iter().collect::<Vec<_>>();
        buckets.sort_by(|a, b| b.0.cmp(a.0));
        let mut items = Vec::with_capacity(5);
        fill_to_cap(&mut items, buckets);
        let key = items.iter().cloned().collect::<String>();

        let mut id_check = id_check.split('[');
        let id = id_check.next().unwrap();
        let check = id_check.next().unwrap().chars().collect::<Vec<_>>();
        let len = check.len();
        let checksum = check[0..len-1].iter().cloned().collect::<String>();

        if key == checksum {
            let new = line.split('-').rev().skip(1).collect::<Vec<_>>();
            let new = new.iter().rev().map(|s| s.to_string()).collect::<Vec<_>>();
            let new = new.join("-");
            acc.push((new, id.parse::<u32>().unwrap()));
            acc
        } else {
            acc
        }
    })
}

/// Part 1!
fn one(content: &String) -> u32 {
    let valid = filter_valid(content);
    valid.iter().fold(0, |acc, line| acc + line.1 )
}


/// Return a char that's been cycled forward `n` chars,
/// only through the alphabet (a-z) and '-' => ' '
pub fn cycle(c: char, n: u32) -> char {
    let start = c as u8;
    let offset = 122 - start;
    if n as u8 <= offset {
        return (start + n as u8) as char;
    }
    let remaining = n - offset as u32;
    let diff = remaining % 26;
    if diff == 0 { 'z' }
    else { (96 + diff as u8) as char }
}

/// Part 2!
fn two(content: &String) -> Option<u32> {
    let valid = filter_valid(content);
    for &(ref message, n) in valid.iter() {
        let msg = message.chars().map(|c| {
            if c == '-' {
                ' '
            } else {
                cycle(c, n)
            }
        }).collect::<String>();
        if msg.contains("northpole") {
            //println!("{:?}", msg);
            return Some(n);
        }
    }
    None
}

pub fn eval(content: &String, part: Part) -> u32 {
    match part {
        Part::One => one(content),
        Part::Two => two(content).unwrap(),
    }
}
