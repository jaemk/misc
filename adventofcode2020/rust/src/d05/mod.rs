use crate::utils::err;
use crate::utils::file;

struct Seat<'a> {
    #[allow(unused)]
    code: &'a str,
    row: usize,
    col: usize,
}
impl<'a> Seat<'a> {
    fn id(&self) -> usize {
        Self::get_id(self.row, self.col)
    }
    fn get_id(row: usize, col: usize) -> usize {
        (row * 8) + col
    }
}

#[inline]
fn reduce_code(code: &str, zero: &str, one: &str) -> usize {
    let code = code.replace(zero, "0").replace(one, "1");
    usize::from_str_radix(&code, 2).expect("invaid code")
}

fn parse(input: &str) -> err::Result<Vec<Seat>> {
    Ok(input
        .lines()
        .map(|code| {
            let fbs = &code[..7];
            let row = reduce_code(fbs, "F", "B");
            let lrs = &code[7..];
            let col = reduce_code(lrs, "L", "R");

            let s = Seat { code, row, col };
            Ok(s)
        })
        .collect::<err::Result<Vec<_>>>()?)
}

fn part1(input: &[Seat]) -> err::Result<usize> {
    Ok(input
        .iter()
        .map(|seat| seat.id())
        .max()
        .ok_or("no max...")?)
}

fn part2(input: &[Seat]) -> err::Result<usize> {
    let mut plane = (0..128)
        .map(|_| {
            let mut row = set!(size = 8);
            (0..8).for_each(|col| {
                row.insert(col);
            });
            row
        })
        .collect::<Vec<_>>();

    for seat in input {
        let row = plane.get_mut(seat.row).ok_or("must be there")?;
        row.remove(&seat.col);
    }

    for (row_id, row) in plane.iter().enumerate().skip(1) {
        if row.len() == 1 {
            let col = *row.iter().next().unwrap();
            return Ok(Seat::get_id(row_id, col));
        }
    }
    Err("failed to find seat".into())
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d05.txt")?,
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
    use itertools::Itertools;

    static INPUT: &[(&str, usize, usize, usize)] = &[
        ("FBFBBFFRLR", 44, 5, 357),
        ("BFFFBBFRRR", 70, 7, 567),
        ("FFFBBBFRRR", 14, 7, 119),
        ("BBFFBBFRLL", 102, 4, 820),
    ];

    #[test]
    fn test_p1() {
        let input = INPUT.iter().map(|(s, _, _, _)| s).join("\n");
        let input = parse(&input).expect("parse fail");
        for (seat, (code, row, col, id)) in input.iter().zip(INPUT) {
            assert_eq!(&seat.code, code);
            assert_eq!(&seat.row, row);
            assert_eq!(&seat.col, col);
            assert_eq!(&seat.id(), id);
        }

        assert_eq!(part1(&input).expect("p1 fail"), 820);
    }
}
