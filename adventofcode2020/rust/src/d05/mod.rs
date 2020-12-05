use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;

fn parse(input: &str) -> err::Result<Vec<&str>> {
    Ok(input.lines().collect::<Vec<_>>())
}

fn part1(input: &[&str]) -> err::Result<u64> {
    Ok(input
        .iter()
        .map(|line| {
            let fbs = &line[..7];
            let lrs = &line[7..];

            let mut row_min = 0;
            let mut row_max = 127;
            for b in fbs.as_bytes() {
                let mut change = (row_max - row_min) / 2;
                if *b == b'F' {
                    row_max -= change + 1;
                } else {
                    row_min += change + 1;
                }
            }
            let row = row_min;

            let mut row_min = 0;
            let mut row_max = 7;
            for b in lrs.as_bytes() {
                let mut change = (row_max - row_min) / 2;
                if *b == b'L' {
                    row_max -= change + 1;
                } else {
                    row_min += change + 1;
                }
            }
            let col = row_min;
            (row * 8) + col
        })
        .max()
        .expect("no max..."))
}

fn part2(input: &[&str]) -> err::Result<u64> {
    Ok(1)
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d05.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
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
    static INPUT: &[(&str, u32, u32, u64)] = &[
        ("FBFBBFFRLR", 44, 5, 357),
        ("BFFFBBFRRR", 70, 7, 567),
        ("FFFBBBFRRR", 14, 7, 119),
        ("BBFFBBFRLL", 102, 4, 820),
    ];
    #[test]
    fn test_p1() {
        let input = INPUT.iter().map(|(s, _, _, _)| s).join("\n");
        let input = parse(&input).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 820);
    }

    // #[test]
    // fn test_p2() {
    //     let input = parse(INPUT).expect("parse fail");
    //     assert_eq!(part2(&input).expect("p2 fail"), 336);
    // }
}
