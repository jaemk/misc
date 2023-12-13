use crate::{utils, Result};

/// https://adventofcode.com/2023/day/1
fn part1(input: &str) -> Result<u64> {
    let res = input
        .trim()
        .lines()
        .map(|line| {
            let line = line.chars().filter(|c| c.is_numeric()).collect::<Vec<_>>();
            let (first, last) = (line[0], line.last().unwrap());

            format!("{first}{last}")
                .parse::<u64>()
                .expect("invalid number")
        })
        .sum();
    Ok(res)
}

/// https://adventofcode.com/2023/day/1#part2
fn part2(input: &str) -> Result<u64> {
    let patterns = [
        ("1", 1),
        ("2", 2),
        ("3", 3),
        ("4", 4),
        ("5", 5),
        ("6", 6),
        ("7", 7),
        ("8", 8),
        ("9", 9),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ];

    let mut sum = 0;
    for line in input.trim().lines() {
        let mut first_pat = None;
        let mut last_pat = None;
        let mut first_index = line.len();
        let mut last_index = 0;
        for pat in &patterns {
            if first_index > 0 {
                if let Some(i) = line.find(pat.0) {
                    if i <= first_index {
                        first_index = i;
                        first_pat = Some(pat.1);
                    }
                }
            }
            if last_index < line.len() - 1 {
                if let Some(i) = line.rfind(pat.0) {
                    if i >= last_index {
                        last_index = i;
                        last_pat = Some(pat.1);
                    }
                }
            }
            if first_index == 0 && last_index == line.len() - 1 {
                break;
            }
        }
        match (first_pat, last_pat) {
            (Some(first), Some(last)) => {
                sum += format!("{first}{last}")
                    .parse::<u64>()
                    .expect("invalida number");
            }
            _ => panic!("did not find first and last pattern {line}"),
        };
    }

    Ok(sum)
}

pub async fn run() -> crate::Result<()> {
    let input = time!(
        utils::file::read("../input/d01.txt").await?,
        (ms) -> println!("  -> read[{ms:.3}ms]"),
    );

    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{ms:.3}ms]: {res}");
    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{ms:.3}ms]: {res}");
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    static SAMPLE_INPUT: &str = r##"
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
    "##;

    #[tokio::test]
    async fn test_p1_sample() {
        let res = part1(SAMPLE_INPUT).expect("error in part 1");
        assert_eq!(res, 142);
    }

    #[tokio::test]
    async fn test_p1_real() {
        let res =
            part1(&utils::file::read("../input/d01.txt").await.unwrap()).expect("error in part 1");
        assert_eq!(res, 54953);
    }

    static SAMPLE_INPUT_2: &str = r##"
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
    "##;

    #[tokio::test]
    async fn test_p2_sample() {
        let res = part2(SAMPLE_INPUT_2).expect("error in part 2");
        assert_eq!(res, 281);
    }

    #[tokio::test]
    async fn test_p2_real() {
        let res =
            part2(&utils::file::read("../input/d01.txt").await.unwrap()).expect("error in part 2");
        assert_eq!(res, 53868);
    }
}
