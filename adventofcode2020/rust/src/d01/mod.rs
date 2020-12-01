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

fn part1(input: &[u32]) -> err::Result<u32> {
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

fn part2(input: &[u32]) -> err::Result<u32> {
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

pub fn run() -> err::Result<()> {
    let input = file::read("../input/d01.txt")?;
    let input = parse(&input)?;

    let (ms, res) = time!(part1(&input)?);
    println!("d01 | p1[{}ms]: {}", ms, res);

    let (ms, res) = time!(part2(&input)?);
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
        assert_eq!(part1(&input).expect("p1 fail"), 514579);
    }

    #[test]
    fn test_ex_p2() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part2(&input).expect("p1 fail"), 241861950);
    }
}
