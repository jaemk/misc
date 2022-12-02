use crate::{utils, Result};

fn parse(s: &str) -> Result<Vec<u64>> {
    let mut res = s
        .trim()
        .split("\n\n")
        .map(|section| {
            let meals = section
                .trim()
                .lines()
                .map(|num| Ok(num.parse()?))
                .collect::<Result<Vec<_>>>()?;
            let total = meals.iter().sum();
            Ok(total)
        })
        .collect::<Result<Vec<u64>>>()?;
    res.sort_by(|a, b| b.cmp(a));
    Ok(res)
}

fn part1(input: &[u64]) -> Result<u64> {
    Ok(input[0])
}

fn part2(input: &[u64]) -> Result<u64> {
    Ok(input.iter().take(3).sum())
}

pub async fn run() -> crate::Result<()> {
    let input = time!(
        utils::file::read("../input/d01.txt").await?,
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
    1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
    "##;

    #[tokio::test]
    async fn test_p1_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 24000);
    }

    #[tokio::test]
    async fn test_p1_real() {
        let input =
            parse(&utils::file::read("../input/d01.txt").await.unwrap()).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 71934);
    }

    #[tokio::test]
    async fn test_p2_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 45000);
    }

    #[tokio::test]
    async fn test_p2_real() {
        let input =
            parse(&utils::file::read("../input/d01.txt").await.unwrap()).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 211447);
    }
}
