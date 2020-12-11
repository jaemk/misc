use crate::utils::err;
use crate::utils::file;
use itertools::max;
use std::collections::HashSet;

const FREE: char = 'L';
const FLOOR: char = '.';
const OCCUPIED: char = '#';

#[derive(Clone)]
struct Seats {
    grid: Vec<Vec<char>>,
    next: Vec<Vec<char>>,
}
impl Seats {
    fn display(&self) {
        println!("--------");
        for row in self.next.iter() {
            println!("{:?}", row)
        }
    }
    fn done(&self) -> bool {
        self.grid == self.next
    }
    fn occupied(&self) -> u32 {
        let mut n = 0;
        for row in self.next.iter() {
            for c in row.iter() {
                if *c == OCCUPIED {
                    n += 1;
                }
            }
        }
        n
    }
    fn step(&mut self) {
        self.grid = self.next.clone();
        let height = self.grid.len();
        let width = self.grid[0].len();
        for row in 0..height {
            for col in 0..width {
                let seat = self.grid[row][col];

                let mut occ = 0;
                for (i, j) in adj(row, col, height - 1, width - 1).into_iter() {
                    if self.grid[i][j] == OCCUPIED {
                        occ += 1;
                    }
                }

                if seat == FREE && occ == 0 {
                    self.next[row][col] = OCCUPIED
                } else if seat == OCCUPIED && occ >= 4 {
                    self.next[row][col] = FREE
                }
            }
        }
    }
}

fn adj(row: usize, col: usize, max_row: usize, max_col: usize) -> HashSet<(usize, usize)> {
    let mut s = set!(
        (row.saturating_sub(1), col.saturating_sub(1)),
        (row.saturating_sub(1), col),
        (row, col.saturating_sub(1)),
    );
    if row < max_row {
        s.insert((row.saturating_add(1), col.saturating_sub(1)));
        s.insert((row.saturating_add(1), col));
        if col < max_col {
            s.insert((row.saturating_add(1), col.saturating_add(1)));
        }
    }
    if col < max_col {
        s.insert((row.saturating_sub(1), col.saturating_add(1)));
        s.insert((row, col.saturating_add(1)));
    }
    s.remove(&(row, col));
    s
}

fn parse(input: &str) -> err::Result<Seats> {
    let grid = input
        .trim()
        .lines()
        .map(|row| row.chars().collect())
        .collect::<Vec<_>>();
    Ok(Seats {
        grid: grid.clone(),
        next: grid,
    })
}

fn part1(mut seats: Seats) -> err::Result<u32> {
    let mut runs = 0;
    loop {
        // seats.display();
        if runs > 0 && seats.done() {
            return Ok(seats.occupied());
        }
        seats.step();
        runs += 1;
    }
}

fn part2(input: Seats) -> err::Result<u32> {
    Ok(1)
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d11.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(input.clone())?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(input.clone())?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &str = r##"
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(input.clone()).expect("p1 fail"), 37);
    }

    // #[test]
    // fn test_p2() {
    //     let input = parse(INPUT_2).expect("parse fail");
    //     assert_eq!(part1(&input).expect("p1 fail"), 220);
    // }
}
