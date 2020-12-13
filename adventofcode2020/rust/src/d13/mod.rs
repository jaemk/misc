use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;

fn parse(input: &str) -> err::Result<(u64, Vec<u64>)> {
    let (depart, schedule) = input
        .trim()
        .split_whitespace()
        .collect_tuple()
        .ok_or_else(|| "unexpected format")?;
    let depart = depart.parse()?;
    let schedule = schedule
        .trim()
        .split(',')
        .filter(|n| *n != "x")
        .map(|n| Ok(n.parse()?))
        .collect::<err::Result<Vec<_>>>()?;

    Ok((depart, schedule))
}

fn part1(depart: u64, schedule: &[u64]) -> err::Result<u64> {
    let (id, wait_time) = schedule
        .iter()
        .map(|dep| {
            let wait_time = dep - (depart % dep);
            (*dep, wait_time)
        })
        .min_by_key(|(_, wait_time)| *wait_time)
        .ok_or_else(|| "no min found")?;
    Ok(id * wait_time)
}

fn part2(depart: u64, schedule: &[u64]) -> err::Result<u64> {
    Ok(1)
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d13.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(input.0, &input.1)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(input.0, &input.1)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &str = r##"
939
7,13,x,x,59,x,31,19
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(input.0, &input.1).expect("p1 fail"), 295)
    }

    // #[test]
    // fn test_p2() {
    //     let input = parse(INPUT).expect("parse fail");
    //     assert_eq!(part2(&input).expect("p2 fail"), 26);
    // }
}
