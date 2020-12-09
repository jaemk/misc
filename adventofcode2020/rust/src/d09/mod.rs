use crate::utils::err;
use crate::utils::file;
use itertools::{Itertools, MinMaxResult};
use std::collections::HashSet;

fn parse(input: &str) -> err::Result<Vec<i64>> {
    Ok(input
        .trim()
        .lines()
        .map(|line| Ok(line.parse::<i64>()?))
        .collect::<err::Result<Vec<_>>>()?)
}

fn part1(data: &[i64], preamble_length: usize) -> err::Result<i64> {
    let mut ptr = preamble_length;
    let mut past = data[0..ptr].iter().copied().collect::<HashSet<_>>();
    while ptr < data.len() {
        let candidate = data[ptr];
        let mut valid = false;
        for &p in past.iter() {
            let compliment = candidate - p;
            if compliment != p && past.contains(&compliment) {
                valid = true;
                break;
            }
        }
        if !valid {
            return Ok(candidate);
        }
        let last_of_parts = data[ptr - preamble_length];
        past.remove(&last_of_parts);
        past.insert(candidate);
        ptr += 1;
    }
    Err("no weakness found".into())
}

fn part2(data: &[i64], weakness: i64) -> err::Result<i64> {
    let mut ptr = 0;
    while ptr < data.len() - 1 {
        let mut next = ptr + 1;
        let mut acc = data[ptr];
        while next < data.len() && acc < weakness {
            acc += data[next];
            next += 1;
        }
        if acc == weakness && ptr != next {
            let values = &data[ptr..next];
            if let MinMaxResult::MinMax(min, max) = values.iter().minmax() {
                return Ok(min + max);
            } else {
                return Err(format!("no minmax found for range: {:?}", values).into());
            }
        }
        ptr += 1;
    }
    Err("no encryption weakness found".into())
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d09.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&input, 25)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(&input, res)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &str = r##"
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input, 5).expect("p1 fail"), 127);
    }

    #[test]
    fn test_p2() {
        let input = parse(INPUT).expect("parse fail");
        let weakness = part1(&input, 5).expect("weakness failed");
        assert_eq!(part2(&input, weakness).expect("p2 fail"), 62);
    }
}
