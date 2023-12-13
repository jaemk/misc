use crate::{utils, Result};
use std::str::FromStr;

enum Color {
    R(u64),
    B(u64),
    G(u64),
}
impl FromStr for Color {
    type Err = ();
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (num, col) = s.trim().split_once(' ').unwrap();
        let num = num.trim().parse::<u64>().unwrap();
        Ok(match col.trim() {
            "red" => Color::R(num),
            "blue" => Color::B(num),
            "green" => Color::G(num),
            _ => panic!("unknown color {s}"),
        })
    }
}
struct Game {
    id: u64,
    rounds: Vec<Vec<Color>>,
}

fn parse(s: &str) -> Result<Vec<Game>> {
    let res = s
        .trim()
        .lines()
        .map(|line| {
            let (game, rounds) = line.split_once(':').unwrap();
            let game_id = game.trim_start_matches("Game ").parse().unwrap();
            let rounds = rounds
                .split(';')
                .map(|round| {
                    round
                        .split(',')
                        .map(|play| play.parse::<Color>().unwrap())
                        .collect::<Vec<Color>>()
                })
                .collect::<Vec<_>>();
            Game {
                id: game_id,
                rounds,
            }
        })
        .collect::<Vec<_>>();
    Ok(res)
}

/// https://adventofcode.com/2023/day/2
fn part1(input: &[Game]) -> Result<u64> {
    // only 12 red cubes, 13 green cubes, and 14 blue cubes
    let max_red = 12;
    let max_green = 13;
    let max_blue = 14;
    let mut sum = 0;
    'games: for game in input {
        for round in &game.rounds {
            for color in round {
                match color {
                    Color::R(num) => {
                        if *num > max_red {
                            continue 'games;
                        }
                    }
                    Color::G(num) => {
                        if *num > max_green {
                            continue 'games;
                        }
                    }
                    Color::B(num) => {
                        if *num > max_blue {
                            continue 'games;
                        }
                    }
                }
            }
        }
        sum += game.id;
    }
    Ok(sum)
}

/// https://adventofcode.com/2023/day/2#part2
fn part2(input: &[Game]) -> Result<u64> {
    let mut sum = 0;
    for game in input {
        let mut max_red = 0;
        let mut max_green = 0;
        let mut max_blue = 0;

        for round in &game.rounds {
            for color in round {
                match color {
                    Color::R(num) => {
                        if *num > max_red {
                            max_red = *num;
                        }
                    }
                    Color::G(num) => {
                        if *num > max_green {
                            max_green = *num;
                        }
                    }
                    Color::B(num) => {
                        if *num > max_blue {
                            max_blue = *num;
                        }
                    }
                }
            }
        }
        sum += max_red * max_green * max_blue;
    }
    Ok(sum)
}

pub async fn run() -> Result<()> {
    let input = time!(
        utils::file::read("../input/d02.txt").await?,
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
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    "##;

    #[tokio::test]
    async fn test_p1_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 8);
    }

    #[tokio::test]
    async fn test_p1_real() {
        let input =
            parse(&utils::file::read("../input/d02.txt").await.unwrap()).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 2528);
    }

    #[tokio::test]
    async fn test_p2_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 2286);
    }

    #[tokio::test]
    async fn test_p2_real() {
        let input =
            parse(&utils::file::read("../input/d02.txt").await.unwrap()).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 67363);
    }
}
