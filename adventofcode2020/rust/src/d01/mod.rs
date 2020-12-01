use crate::utils::err;
use crate::utils::file;

use itertools::iproduct;

fn parse(input: &str) -> err::Result<Vec<u32>> {
    Ok(input
        .trim()
        .lines()
        .map(|line| Ok(line.parse::<u32>()?))
        .collect::<err::Result<_>>()?)
}

#[allow(unused)]
mod brute {
    use super::*;
    pub fn part1(input: &[u32]) -> err::Result<u32> {
        for (n1, n2) in iproduct!(input.iter(), input.iter()) {
            if n1 == n2 {
                continue;
            }
            if (n1 + n2) == 2020 {
                return Ok(n1 * n2);
            }
        }
        Err("failed".into())
    }

    pub fn part2(input: &[u32]) -> err::Result<u32> {
        for (n1, n2, n3) in iproduct!(input.iter(), input.iter(), input.iter()) {
            if n1 == n2 || n2 == n3 || n3 == n1 {
                continue;
            }
            if (n1 + n2 + n3) == 2020 {
                return Ok(n1 * n2 * n3);
            }
        }
        Err("failed".into())
    }
}

mod smart {
    use super::*;
    pub fn part1(input: &[u32]) -> err::Result<u32> {
        let size = input.len();
        for i in 0..(size - 1) {
            let current = input[i];
            let want = 2020 - current;

            let n = i + 1;
            let candidates = &input[n..size];
            if candidates.binary_search(&want).is_ok() {
                return Ok(current * want);
            }
        }
        Err("failed".into())
    }

    pub fn part2(input: &[u32]) -> err::Result<u32> {
        let size = input.len();
        for i in 0..(size - 2) {
            let mut next = i + 1;
            while next < (size - 1) {
                let current_a = input[i];
                let current_b = input[next];
                let want = 2020_u32.saturating_sub(current_a).saturating_sub(current_b);
                if want > 0 {
                    let n = next + 1;
                    let candidates = &input[n..size];
                    if candidates.binary_search(&want).is_ok() {
                        return Ok(current_a * current_b * want);
                    }
                }
                next += 1;
            }
        }
        Err("failed".into())
    }
}

pub fn run() -> err::Result<()> {
    let input = file::read("../input/d01.txt")?;
    let mut input = parse(&input)?;

    // let (ms, res) = time!(brute::part1(&input)?);
    // println!("d01 | p1[{}ms]: {}", ms, res);
    // let (ms, res) = time!(brute::part2(&input)?);
    // println!("d01 | p2[{}ms]: {}", ms, res);

    input.sort_unstable();
    let (ms, res) = time!(smart::part1(&input)?);
    println!("d01 | p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(smart::part2(&input)?);
    println!("d01 | p2[{}ms]: {}", ms, res);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    static INPUT: &str = r##"
1721
979
366
299
675
1456
"##;

    #[test]
    fn test_ex_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(brute::part1(&input).expect("p1 fail"), 514579);
    }

    #[test]
    fn test_ex_p2() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(brute::part2(&input).expect("p2 fail"), 241861950);
    }

    #[test]
    fn test_ex_p1_smart() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(smart::part1(&input).expect("p1 fail"), 514579);
    }

    #[test]
    fn test_ex_p2_smart() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(smart::part2(&input).expect("p2 fail"), 241861950);
    }
}
