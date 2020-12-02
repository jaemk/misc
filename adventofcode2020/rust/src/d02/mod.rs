use crate::utils::err;
use crate::utils::file;

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
        .lines()
        .map(|line| {
            let mut s = line.split_whitespace();
            let mut min_max_iter = s.next().unwrap().split('-');
            let min = min_max_iter.next().unwrap().parse::<u32>()?;
            let max = min_max_iter.next().unwrap().parse::<u32>()?;
            let character = s.next().unwrap().chars().next().unwrap();
            let pw = s.next().unwrap();
            Ok(Pw {
                min,
                max,
                character,
                pw,
            })
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
    #[test]
    fn test_p1() {
        assert!(true)
    }
}
