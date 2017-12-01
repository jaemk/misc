/*! Day1
http://adventofcode.com/2017/day/1

*/

static INPUT: &'static str = include_str!("../input.txt");


fn solve1(input: &str) -> u32 {
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


fn solve2(input: &str) -> u32 {
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


pub fn main() {
    println!("day1-part1: {}", solve1(INPUT));
    println!("day1-part2: {}", solve2(INPUT));
}


#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test1() {
        [("1122", 3), ("1111", 4), ("1234", 0), ("91212129", 9)].iter().for_each(|&(input, ans)| {
            assert_eq!(solve1(input), ans, "input `{}` expected output `{}`", input, ans);
        })
    }

    #[test]
    fn test2() {
        [("1212", 6), ("1221", 0), ("123425", 4), ("123123", 12), ("12131415", 4)].iter().for_each(|&(input, ans)| {
            assert_eq!(solve2(input), ans, "input `{}` expected output `{}`", input, ans);
        })
    }
}
