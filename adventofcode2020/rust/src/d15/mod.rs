use crate::utils::err;
use crate::utils::file;
use std::collections::{HashMap, VecDeque};

fn parse(input: &str) -> err::Result<Vec<u32>> {
    Ok(input
        .trim()
        .split(',')
        .map(|num| Ok(num.trim().parse::<u32>()?))
        .collect::<err::Result<Vec<_>>>()?)
}

fn do_part1(input: &[u32], end: u32) -> err::Result<u32> {
    let mut mem: HashMap<u32, VecDeque<u32>> = map!(size = 100);
    let mut turn = 1;
    let mut spoooooken = Vec::with_capacity(30000000);
    let mut last_spoken = 0;
    for n in input {
        let mut v = VecDeque::with_capacity(3);
        v.push_front(turn);
        mem.insert(*n, v);
        turn += 1;
        last_spoken = *n;
    }
    while turn <= end {
        let speak = {
            let spoken_at = mem.entry(last_spoken).or_default();
            if spoken_at.len() == 1 {
                0
            } else {
                assert_eq!(spoken_at.len(), 2);
                spoken_at[0] - spoken_at[1]
            }
        };
        let spoken = mem.entry(speak).or_default();
        spoken.push_front(turn);
        spoken.truncate(2);
        last_spoken = speak;
        turn += 1;
        spoooooken.push(speak);
        check_cycle(&spoooooken);
    }
    Ok(last_spoken)
}

fn part1(input: &[u32]) -> err::Result<u32> {
    do_part1(input, 2020)
}

fn check_cycle(vals: &[u32]) {
    if vals.len() % 2 == 0 {
        return;
    }
}

fn part2(input: &[u32]) -> err::Result<u32> {
    do_part1(input, 30000000)
}

pub fn run() -> err::Result<()> {
    let raw_input = time!(
        file::read("../input/d15.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );

    let input = time!(
        parse(&raw_input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{}ms]: {}", ms, res);

    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &[(&str, u32)] = &[
        ("0,3,6", 436),
        ("1,3,2", 1),
        ("2,1,3", 10),
        ("1,2,3", 27),
        ("2,3,1", 78),
        ("3,2,1", 438),
        ("3,1,2", 1836),
    ];

    #[test]
    fn test_p1() {
        for &(input, ans) in INPUT {
            let input = parse(input).expect("parse fail");
            assert_eq!(part1(&input).expect("p1 fail"), ans);
        }
    }

    static INPUT2: &[(&str, u32)] = &[
        ("0,3,6", 175594),
        ("1,3,2", 2578),
        ("2,1,3", 3544142),
        ("1,2,3", 261214),
        ("2,3,1", 6895259),
        ("3,2,1", 18),
        ("3,1,2", 362),
    ];

    #[test]
    fn test_p2() {
        for &(input, ans) in INPUT2 {
            let input = parse(input).expect("parse fail");
            assert_eq!(part2(&input).expect("p2 fail"), ans);
        }
    }
}
