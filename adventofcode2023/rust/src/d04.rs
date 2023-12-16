use crate::{utils, Result};
use std::collections::HashSet;

struct Card {
    number: u64,
    #[allow(unused)]
    winners: HashSet<u64>,
    #[allow(unused)]
    chosen: HashSet<u64>,
    won: HashSet<u64>,
}
impl Card {
    fn score(&self) -> u64 {
        let wins = self.won.len();
        if wins > 0 {
            let exp = self.won.len() as u32 - 1;
            2u64.pow(exp)
        } else {
            0
        }
    }
}

fn parse(s: &str) -> Result<Vec<Card>> {
    let res = s
        .trim()
        .lines()
        .map(|line| {
            let (card_number, numbers) = line.split_once(':').unwrap();
            let number = card_number.strip_prefix("Card").unwrap().trim().parse()?;
            let (winners, chosen) = numbers.split_once('|').unwrap();
            let winners = winners
                .split_whitespace()
                .map(|s| Ok(s.parse::<u64>()?))
                .collect::<Result<HashSet<u64>>>()?;
            let chosen = chosen
                .split_whitespace()
                .map(|s| Ok(s.parse::<u64>()?))
                .collect::<Result<HashSet<u64>>>()?;
            let won = chosen
                .iter()
                .filter_map(|n| winners.get(n).copied())
                .collect::<HashSet<u64>>();
            Ok(Card {
                number,
                winners,
                chosen,
                won,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(res)
}

/// https://adventofcode.com/2023/day/4
fn part1(input: &[Card]) -> Result<u64> {
    let sum = input.iter().map(|c| c.score()).sum();
    Ok(sum)
}

/// https://adventofcode.com/2023/day/4#part2
fn part2(input: &[Card]) -> Result<u64> {
    let mut card_counts = map!();
    for card in input {
        let count = {
            let count = card_counts.entry(card.number).or_insert(0);
            *count += 1;
            *count
        };
        let wins = card.won.len() as u64;
        (card.number + 1..=card.number + wins).for_each(|n| {
            let e = card_counts.entry(n).or_insert(0);
            *e += count;
        });
    }
    let sum = card_counts.values().copied().sum();
    Ok(sum)
}

pub async fn run() -> Result<()> {
    let input = time!(
        utils::file::read("../input/d04.txt").await?,
        (ms) -> println!("  -> read[{ms:.3}ms]"),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{ms:.3}ms]"),
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
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    "##;

    #[tokio::test]
    async fn test_p1_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 13);
    }

    #[tokio::test]
    async fn test_p1_real() {
        let input =
            parse(&utils::file::read("../input/d04.txt").await.unwrap()).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 21919);
    }

    #[tokio::test]
    async fn test_p2_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 30);
    }

    #[tokio::test]
    async fn test_p2_real() {
        let input =
            parse(&utils::file::read("../input/d04.txt").await.unwrap()).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 9881048);
    }
}
