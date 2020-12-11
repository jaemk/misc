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
    height: usize,
    width: usize,
}
impl Seats {
    fn display(&self) {
        println!("--------");
        for row in self.next.iter() {
            println!("{:?}", row)
        }
    }

    #[inline]
    fn is_done(&self) -> bool {
        self.grid == self.next
    }

    #[inline]
    fn count_occupied(&self) -> u32 {
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

    #[inline]
    fn count_adj_occupied(&self, row: usize, col: usize) -> u32 {
        // gross
        let mut count = 0;
        if row > 0 {
            if col > 0 {
                if self.grid[row - 1][col - 1] == OCCUPIED {
                    count += 1;
                }
            }
            if self.grid[row - 1][col] == OCCUPIED {
                count += 1;
            }
            if col < self.width - 1 {
                if self.grid[row - 1][col + 1] == OCCUPIED {
                    count += 1;
                }
            }
        }
        if row < self.height - 1 {
            if col > 0 {
                if self.grid[row + 1][col - 1] == OCCUPIED {
                    count += 1;
                }
            }
            if self.grid[row + 1][col] == OCCUPIED {
                count += 1;
            }
            if col < self.width - 1 {
                if self.grid[row + 1][col + 1] == OCCUPIED {
                    count += 1;
                }
            }
        }
        if col > 0 {
            if self.grid[row][col - 1] == OCCUPIED {
                count += 1;
            }
        }
        if col < self.width - 1 {
            if self.grid[row][col + 1] == OCCUPIED {
                count += 1;
            }
        }
        count
    }

    fn step(&mut self) {
        self.grid = self.next.clone();
        for row in 0..self.height {
            for col in 0..self.width {
                let seat = self.grid[row][col];

                let occ = self.count_adj_occupied(row, col);
                if seat == FREE && occ == 0 {
                    self.next[row][col] = OCCUPIED
                } else if seat == OCCUPIED && occ >= 4 {
                    self.next[row][col] = FREE
                }
            }
        }
    }
}

fn parse(input: &str) -> err::Result<Seats> {
    let grid = input
        .trim()
        .lines()
        .map(|row| row.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    Ok(Seats {
        height: grid.len(),
        width: grid[0].len(),
        grid: grid.clone(),
        next: grid,
    })
}

fn part1(mut seats: Seats) -> err::Result<u32> {
    let mut runs = 0;
    loop {
        // seats.display();
        if runs > 0 && seats.is_done() {
            return Ok(seats.count_occupied());
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
