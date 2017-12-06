use std::collections::HashMap;

static INPUT: &'static str = include_str!("../input.txt");


mod p1 {
    use super::*;

    pub fn valid_passphrase(s: &str) -> bool {
        let mut map = HashMap::new();
        for chunk in s.split_whitespace() {
            let e = map.entry(chunk).or_insert(0);
            if *e > 0 { return false }
            *e += 1;
        }
        true
    }

    pub fn solve(input: &str) -> usize {
        input.trim().lines()
            .map(|phrase| if valid_passphrase(phrase) { 1 } else { 0 })
            .sum()
    }
}


mod p2 {
    use super::*;

    fn freq(s: &str) -> HashMap<char, usize> {
        s.chars().fold(HashMap::new(), |mut map, c| {
            {
                let e = map.entry(c).or_insert(0);
                *e += 1;
            }
            map
        })
    }

    pub fn valid_passphrase(pw: &str) -> bool {
        let words = pw.split_whitespace()
            .map(|word| freq(word))
            .collect::<Vec<_>>();
        let n = words.len();
        for (i, word) in words[..n-1].iter().enumerate() {
            for other in &words[i+1..] {
                if word == other { return false }
            }
        }
        true
    }

    pub fn solve(input: &str) -> usize {
        input.trim().lines()
            .map(|phrase| if valid_passphrase(phrase) { 1 } else { 0 })
            .sum()
    }
}


pub fn main() {
    println!("d4-p1: (num valid): {}", p1::solve(INPUT));
    println!("d4-p2: (num valid): {}", p2::solve(INPUT));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        [
            ("aa bb cc dd ee", true),
            ("aa bb cc dd aa", false),
            ("aa bb cc dd aaa", true),
        ].iter().for_each(|&(input, expected)| {
                assert_eq!(p1::valid_passphrase(input), expected, "Expected `{}` valid={}", input, expected);
            })
    }

    #[test]
    fn p2() {
        [
            ("abcde fghij", true),
            ("abcde xyz ecdab", false),
            ("a ab abc abd abf abj", true),
            ("iiii oiii ooii oooi oooo", true),
            ("oiii ioii iioi iiio", false),
        ].iter().for_each(|&(input, expected)| {
            assert_eq!(p2::valid_passphrase(input), expected, "Expected `{}` valid={}", input, expected);
        })
    }
}

