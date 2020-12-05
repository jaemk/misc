use crate::utils::err;
use crate::utils::file;

struct Seat<'a> {
    #[allow(unused)]
    code: &'a str,
    #[allow(unused)]
    row: usize,
    #[allow(unused)]
    col: usize,
    id: usize,
}

#[inline]
fn make_id(row: usize, col: usize) -> usize {
    // id's are a sequence of ints since we're multiplying by
    // 8 and col is in range 0..8
    (row * 8) + col
}

const BIT_FLAGS: &[usize] = &[64, 32, 16, 8, 4, 2, 1];

#[inline]
fn reduce_code(code: &str, one: u8) -> usize {
    // FBFBBFF -> 0101100
    // LRR -> 011
    let code_len = code.len();
    let flags_len = BIT_FLAGS.len();
    let starting_flag = flags_len - code_len;

    // FBFBBFF -> [64, 32, 16, 8, 4, 2, 1]
    // LRR -> [4, 2, 1]
    let bit_flags = &BIT_FLAGS[starting_flag..];

    code.bytes()
        .zip(bit_flags.iter())
        .fold(0, |acc, (char_flag, bit_flag)| {
            if char_flag == one {
                acc | bit_flag
            } else {
                acc
            }
        })
}

fn parse(input: &str) -> err::Result<Vec<Seat>> {
    let mut seats = Vec::with_capacity(1000);
    seats.extend(input.lines().map(|code| {
        let fbs = &code[..7];
        let row = reduce_code(fbs, b'B');
        let lrs = &code[7..];
        let col = reduce_code(lrs, b'R');
        let id = make_id(row, col);
        Seat { code, row, col, id }
    }));
    seats.sort_unstable_by_key(|s| s.id);
    Ok(seats)
}

fn part1(input: &[Seat]) -> err::Result<usize> {
    Ok(input[input.len() - 1].id)
}

fn part2(seats: &[Seat]) -> err::Result<usize> {
    let first_seat_id = seats[0].id;
    let last_seat_id = seats[seats.len() - 1].id;
    for seat_id in first_seat_id..last_seat_id + 1 {
        let seat_index = seat_id - first_seat_id;
        if seats[seat_index].id != seat_id {
            // it's free
            return Ok(seat_id);
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
        let input = INPUT.iter().map(|&(s, _, _, _)| s).join("\n");
        let seats = parse(&input).expect("parse fail");

        let mut sorted_input = INPUT.to_vec();
        sorted_input.sort_unstable_by_key(|&(_, _, _, id)| id);
        for (seat, (code, row, col, id)) in seats.iter().zip(sorted_input) {
            assert_eq!(seat.code, code);
            assert_eq!(seat.row, row);
            assert_eq!(seat.col, col);
            assert_eq!(seat.id, id);
        }

        assert_eq!(part1(&seats).expect("p1 fail"), 820);
    }
}
