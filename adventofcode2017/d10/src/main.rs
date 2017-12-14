/*!
http://adventofcode.com/2017/day/10
*/
extern crate data_encoding;
extern crate d10;

use data_encoding::HEXLOWER;
use d10::{Ring, RingHasher, knot};


static INPUT: &'static str = "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110";


fn part1(input: &str) -> usize {
    let lengths = input.trim()
        .split(",")
        .map(str::trim)
        .map(|s| s.parse::<u8>().expect("Invalid int"))
        .collect::<Vec<_>>();
    let mut ring = Ring::with_size(256);
    let mut hasher = RingHasher::new(&lengths);
    hasher.hash_step(&mut ring);
    ring[0] * ring[1]
}


fn part2(input: &str) -> String {
    let bytes = knot::hash(input);
    HEXLOWER.encode(bytes.as_slice())
}


pub fn main() {
    println!("d10-p1: {}", part1(INPUT));
    println!("d10-p2: {}", part2(INPUT));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        let lengths = [3, 4, 1, 5];
        let mut ring = Ring::with_size(5);
        let mut hasher = RingHasher::new(&lengths);
        hasher.hash_step(&mut ring);
        assert_eq!(ring[0] * ring[1], 12);
    }

    #[test]
    fn p2() {
        [
            ("",            "a2582a3a0e66e6e86e3812dcb672a272"),
            ("AoC 2017",    "33efeb34ea91902bb2f59c9920caa6cd"),
            ("1,2,3",       "3efbe78a8d82f29979031a4aa0b16a9d"),
            ("1,2,4",       "63960835bcdc130f0b66d7ff4f6a5a8e"),
        ].iter().for_each(|&(input, expected)| {
            assert_eq!(part2(input), expected, "error hashing: {:?}", input);
        })
    }
}

