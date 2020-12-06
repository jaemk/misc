use crate::utils::err;
use crate::utils::file;
use std::collections::HashSet;

struct Group {
    unique_answer_count: usize,
    joint_answer_count: usize,
}

fn count_unique(resp: &str) -> usize {
    let set = resp
        .split_whitespace()
        .map(|line| line.chars())
        .flatten()
        .collect::<HashSet<_>>();
    set.len()
}

fn count_joint(resp: &str) -> usize {
    let set = resp
        .split_whitespace()
        .map(|line| line.chars().collect::<HashSet<_>>())
        .enumerate()
        .fold(set!(), |acc, (i, next)| {
            if i == 0 {
                next
            } else {
                acc.intersection(&next).cloned().collect()
            }
        });
    set.len()
}

fn parse(input: &str) -> err::Result<Vec<Group>> {
    Ok(input
        .trim()
        .split("\n\n")
        .map(|response| Group {
            unique_answer_count: count_unique(response),
            joint_answer_count: count_joint(response),
        })
        .collect())
}

fn part1(groups: &[Group]) -> err::Result<usize> {
    Ok(groups.iter().map(|g| g.unique_answer_count).sum())
}

fn part2(groups: &[Group]) -> err::Result<usize> {
    Ok(groups.iter().map(|g| g.joint_answer_count).sum())
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d06.txt")?,
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
    static INPUT: &str = r##"
abc

a
b
c

ab
ac

a
a
a
a

b
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 11);
    }

    #[test]
    fn test_p2() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part2(&input).expect("p2 fail"), 6);
    }
}
