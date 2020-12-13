use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;

mod part1 {
    use super::*;

    pub fn parse(input: &str) -> err::Result<(u64, Vec<u64>)> {
        let (depart, schedule) = input
            .trim()
            .split_whitespace()
            .collect_tuple()
            .ok_or("unexpected format")?;
        let depart = depart.parse()?;
        let schedule = schedule
            .trim()
            .split(',')
            .filter(|n| *n != "x")
            .map(|n| Ok(n.parse()?))
            .collect::<err::Result<Vec<_>>>()?;

        Ok((depart, schedule))
    }

    pub fn solve(depart: u64, schedule: &[u64]) -> err::Result<u64> {
        let (id, wait_time) = schedule
            .iter()
            .map(|dep| {
                let wait_time = dep - (depart % dep);
                (*dep, wait_time)
            })
            .min_by_key(|(_, wait_time)| *wait_time)
            .ok_or("no min found")?;
        Ok(id * wait_time)
    }
}

mod part2 {
    use super::*;

    pub fn parse(input: &str) -> err::Result<Vec<(u64, u64)>> {
        let (_, schedule) = input
            .trim()
            .split_whitespace()
            .collect_tuple()
            .ok_or("unexpected format")?;
        let schedule = schedule
            .trim()
            .split(',')
            .enumerate()
            .filter(|(_, n)| *n != "x")
            .map(|(i, n)| Ok((i as u64, n.parse()?)))
            .collect::<err::Result<Vec<_>>>()?;
        Ok(schedule)
    }

    pub fn solve(schedule: &[(u64, u64)]) -> err::Result<u64> {
        let (mut pos, mut step) = schedule[0];
        for &(index, next) in &schedule[1..] {
            // keep stepping forward by "step" until we fit this next value
            while (pos + index) % next != 0 {
                pos += step;
            }
            // now that we fit this next value, we have to include it in our step value
            step *= next;
        }
        Ok(pos)
    }
}

pub fn run() -> err::Result<()> {
    let raw_input = time!(
        file::read("../input/d13.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );

    let input = time!(
        part1::parse(&raw_input)?,
        (ms) -> println!("  -> parse-p1[{}ms]", ms),
    );
    let (ms, res) = time!(part1::solve(input.0, &input.1)?);
    println!("  -> p1[{}ms]: {}", ms, res);

    let input = time!(
        part2::parse(&raw_input)?,
        (ms) -> println!("  -> parse-p1[{}ms]", ms),
    );
    let (ms, res) = time!(part2::solve(&input)?);
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
        let input = part1::parse(INPUT).expect("parse fail");
        assert_eq!(part1::solve(input.0, &input.1).expect("p1 fail"), 295)
    }

    #[test]
    fn test_p2() {
        let input = part2::parse(INPUT).expect("parse fail");
        assert_eq!(part2::solve(&input).expect("p2 fail"), 1068781);
    }
}
