use crate::utils::err;
use crate::utils::file;
use std::arch::x86_64::_popcnt64;

struct Group {
    unique_answer_count: usize,
    joint_answer_count: usize,
}

fn parse(input: &str) -> err::Result<Vec<Group>> {
    Ok(input
        .trim()
        .split("\n\n")
        .map(|response| {
            let mut unique_answers = 0;
            let mut joint_answers = 0;

            for (i, individual) in response.lines().enumerate() {
                let mut individual_answers = 0;
                for b in individual.bytes() {
                    let pos = b - b'a';
                    individual_answers |= 1 << pos;
                }
                unique_answers |= individual_answers;
                if i == 0 {
                    joint_answers |= individual_answers;
                } else {
                    joint_answers &= individual_answers;
                }
            }
            Group {
                unique_answer_count: unsafe { _popcnt64(unique_answers) as usize },
                joint_answer_count: unsafe { _popcnt64(joint_answers) as usize },
            }
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
