use crate::{utils, Result};

fn parse(s: &str) -> Result<Vec<(String, String)>> {
    let res = s
        .trim()
        .lines()
        .map(|line| {
            let parts = line.split(' ').collect::<Vec<_>>();
            (parts[0].trim().to_string(), parts[1].trim().to_string())
        })
        .collect::<Vec<_>>();
    Ok(res)
}

fn part1(input: &[(String, String)]) -> Result<u32> {
    fn is_win(them: &str, us: &str) -> bool {
        (us == "X" && them == "C") // rock / scissors
            || (us == "Y" && them == "A") // paper / rock
            || (us == "Z" && them == "B") // scissors / paper
    }
    fn is_draw(them: &str, us: &str) -> bool {
        (us == "X" && them == "A") || (us == "Y" && them == "B") || (us == "Z" && them == "C")
    }
    let res = input.iter().fold(0, |acc, (them, us)| {
        acc + if is_win(them, us) {
            6 + value(us)
        } else if is_draw(them, us) {
            3 + value(us)
        } else {
            value(us)
        }
    });
    Ok(res)
}

fn value(us: &str) -> u32 {
    match us {
        "X" => 1,
        "Y" => 2,
        "Z" => 3,
        _ => 0,
    }
}

fn part2(input: &[(String, String)]) -> Result<u32> {
    fn lose(them: &str) -> &'static str {
        match them {
            "A" => "Z",
            "B" => "X",
            "C" => "Y",
            _ => ".",
        }
    }
    fn draw(them: &str) -> &'static str {
        match them {
            "A" => "X",
            "B" => "Y",
            "C" => "Z",
            _ => ".",
        }
    }
    fn win(them: &str) -> &'static str {
        match them {
            "A" => "Y",
            "B" => "Z",
            "C" => "X",
            _ => ".",
        }
    }
    let mut res = 0;
    for (them, outcome) in input.iter() {
        if outcome == "X" {
            res += value(lose(them));
        } else if outcome == "Y" {
            res += 3 + value(draw(them));
        } else if outcome == "Z" {
            res += 6 + value(win(them));
        } else {
            return Err(format!("invalid outcome: {outcome}").into());
        }
    }
    Ok(res)
}

pub async fn run() -> crate::Result<()> {
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
A Y
B X
C Z
    "##;

    #[tokio::test]
    async fn test_p1_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 15);
    }

    #[tokio::test]
    async fn test_p1_real() {
        let input =
            parse(&utils::file::read("../input/d02.txt").await.unwrap()).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 10310);
    }

    #[tokio::test]
    async fn test_p2_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 12);
    }

    #[tokio::test]
    async fn test_p2_real() {
        let input =
            parse(&utils::file::read("../input/d02.txt").await.unwrap()).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 14859);
    }
}
