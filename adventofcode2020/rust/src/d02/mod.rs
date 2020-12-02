use crate::utils::err;
use crate::utils::file;

use itertools::Itertools;

struct Pw<'a> {
    min: u32,
    max: u32,
    character: char,
    pw: &'a str,
}

impl<'a> Pw<'a> {
    fn is_valid(&self) -> bool {
        let c_count = self
            .pw
            .chars()
            .map(|c| if c == self.character { 1 } else { 0 })
            .sum::<u32>();
        c_count >= self.min && c_count <= self.max
    }

    fn is_valid_2(&self) -> bool {
        let c = self.character as u8;
        let bytes = self.pw.as_bytes();
        let mut count = 0;
        if bytes[(self.min - 1) as usize] == c {
            count += 1;
        }
        if bytes[(self.max - 1) as usize] == c {
            count += 1;
        }
        count == 1
    }
}

fn parse(input: &str) -> err::Result<Vec<Pw>> {
    Ok(input
        .trim()
        .lines()
        .map(|line| {
            // 1-3 a: abcde
            if let Some((min_max_part, char_part, pw_part)) =
                line.split_whitespace().collect_tuple()
            {
                if let Some((min_s, max_s)) = min_max_part.split('-').collect_tuple() {
                    let min = min_s.parse::<u32>()?;
                    let max = max_s.parse::<u32>()?;

                    let character = char_part
                        .chars()
                        .next()
                        .ok_or_else(|| format!("invalid char part: {}", char_part))?;
                    let pw = pw_part;
                    Ok(Pw {
                        min,
                        max,
                        character,
                        pw,
                    })
                } else {
                    Err(format!("invalid min/max format: {}", min_max_part).into())
                }
            } else {
                Err(format!("invalid line format: {}", line).into())
            }
        })
        .collect::<err::Result<Vec<_>>>()?)
}

fn part1(input: &[Pw]) -> err::Result<u32> {
    Ok(input
        .iter()
        .map(|pw| if pw.is_valid() { 1 } else { 0 })
        .sum())
}

fn part2(input: &[Pw]) -> err::Result<u32> {
    Ok(input
        .iter()
        .map(|pw| if pw.is_valid_2() { 1 } else { 0 })
        .sum())
}

pub fn run() -> err::Result<()> {
    let input = file::read("../input/d02.txt")?;
    let input = parse(&input)?;

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
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 2);
    }
    #[test]
    fn test_p2() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part2(&input).expect("p2 fail"), 1);
    }
}
