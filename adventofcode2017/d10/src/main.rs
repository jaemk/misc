/*!
http://adventofcode.com/2017/day/10
*/
extern crate data_encoding;

use data_encoding::HEXLOWER;
use std::ops;


static INPUT: &'static str = "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110";


#[derive(Debug)]
struct Ring {
    inner: Vec<usize>,
}
impl Ring {
    fn with_capacity(n: usize) -> Self {
        Self {
            inner: (0..n).collect::<Vec<_>>(),
        }
    }

    fn get_range(&self, mut start: usize, size: usize) -> Vec<usize> {
        let len = self.inner.len();
        assert!(size <= len);
        while start >= len { start -= len; }
        if (start + size) >= len {
            let mut v = self.inner[start..].to_vec();
            let n_from_front = (start + size) - len ;
            v.extend_from_slice(&self.inner[..n_from_front]);
            v
        } else {
            self.inner[start..start+size].to_vec()
        }
    }

    fn set_range(&mut self, start: usize, buf: &[usize]) {
        let inner_len = self.inner.len();
        assert!(buf.len() <= inner_len);
        for (i, val) in buf.iter().enumerate() {
            let mut ind = start + i;
            while ind >= inner_len { ind -= inner_len; }
            self.inner[ind] = *val;
        }
    }
}
impl ops::Deref for Ring {
    type Target = Vec<usize>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}


struct RingHasher<'a> {
    ind: usize,
    skip: usize,
    lengths: &'a [u8],
}
impl<'a> RingHasher<'a> {
    fn new(lengths: &'a [u8]) -> Self {
        Self {
            ind: 0,
            skip:0,
            lengths
        }
    }

    fn hash_step(&mut self, ring: &mut Ring) {
        for len in self.lengths {
            let len = *len as usize;
            let mut slice = ring.get_range(self.ind, len);
            slice.reverse();
            ring.set_range(self.ind, &slice);
            self.ind += len + self.skip;
            self.skip += 1;
        }
    }

    fn knot_hash(&mut self, ring: &mut Ring) -> String {
        for _ in 0..64 {
            self.hash_step(ring);
        }
        let sparse = ring.chunks(16).map(|chunk| {
            chunk.iter().fold(0, |acc, n| acc ^ n) as u8
        }).collect::<Vec<u8>>();
        HEXLOWER.encode(sparse.as_slice())
    }
}


fn part1(input: &str) -> usize {
    let lengths = input.trim()
        .split(",")
        .map(str::trim)
        .map(|s| s.parse::<u8>().expect("Invalid int"))
        .collect::<Vec<_>>();
    let mut ring = Ring::with_capacity(256);
    let mut hasher = RingHasher::new(&lengths);
    hasher.hash_step(&mut ring);
    ring[0] * ring[1]
}


fn part2(input: &str) -> String {
    let mut lengths = input.as_bytes().to_vec();
    lengths.extend_from_slice(&[17, 31, 73, 47, 23]);

    let mut ring = Ring::with_capacity(256);
    let mut hasher = RingHasher::new(&lengths);
    hasher.knot_hash(&mut ring)
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
        let mut ring = Ring::with_capacity(5);
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

