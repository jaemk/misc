use crate::utils::err;
use crate::utils::file;

// macro to run an expression against all surrounding points
// similar to the result of itertools::iproduct!(&[-1, 0, -1], &[-1, 0, -1]))
macro_rules! with_surrounding {
    (
        // current point and the grid's height/width
        ($row:expr, $col:expr, $height:expr, $width:expr),
        // identity to bind the change in row/col to
        ($dr:ident, $dc:ident)
        // expression to execute with dr/dc set
        -> $ex:expr) => {{
            if $row > 0 {
                if $col > 0 {
                    // left up
                    let $dr = -1;
                    let $dc = -1;
                    $ex
                }
                // up
                let $dr = -1;
                let $dc = 0;
                $ex
                if $col < $width - 1 {
                    // right up
                    let $dr = -1;
                    let $dc = 1;
                    $ex
                }
            }
            if $row < $height - 1 {
                if $col > 0 {
                    // down left
                    let $dr = 1;
                    let $dc = -1;
                    $ex
                }
                // down
                let $dr = 1;
                let $dc = 0;
                $ex
                if $col < $width - 1 {
                    // down right
                    let $dr = 1;
                    let $dc = 1;
                    $ex
                }
            }
            if $col > 0 {
                // left
                let $dr = 0;
                let $dc = -1;
                $ex
            }
            if $col < $width - 1 {
                // right
                let $dr = 0;
                let $dc = 1;
                $ex
            }
    }};
}

const FREE: char = 'L';
const OCCUPIED: char = '#';
const FLOOR: char = '.';

#[derive(Clone)]
struct Seats {
    grid: Vec<Vec<char>>,
    next: Vec<Vec<char>>,
    height: usize,
    width: usize,
}
impl Seats {
    #[allow(unused)]
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
        let mut count = 0;
        let r = row as isize;
        let c = col as isize;
        with_surrounding!(
            (row, col, self.height, self.width), (dr, dc) -> {
                let r = r + dr;
                let c = c + dc;
                if self.grid[r as usize][c as usize] == OCCUPIED {
                    count += 1;
                }
            }
        );
        count
    }

    fn step_part1(&mut self) {
        self.grid = self.next.clone();
        for row in 0..self.height {
            for col in 0..self.width {
                let seat = self.grid[row][col];
                if seat == FLOOR {
                    continue;
                }

                let occ = self.count_adj_occupied(row, col);
                if seat == FREE && occ == 0 {
                    self.next[row][col] = OCCUPIED
                } else if seat == OCCUPIED && occ >= 4 {
                    self.next[row][col] = FREE
                }
            }
        }
    }

    fn look_is_occupied(&self, row: usize, col: usize, row_d: isize, col_d: isize) -> bool {
        let mut row = row as isize + row_d;
        let mut col = col as isize + col_d;
        let max_h = self.height as isize - 1;
        let max_w = self.width as isize - 1;
        while row >= 0 && col >= 0 && row <= max_h && col <= max_w {
            let r = row as usize;
            let c = col as usize;
            if self.grid[r][c] == OCCUPIED {
                return true;
            }
            if self.grid[r][c] == FREE {
                return false;
            }
            row += row_d;
            col += col_d;
        }
        false
    }

    fn count_visible_occupied(&self, row: usize, col: usize) -> u32 {
        let mut count = 0;
        with_surrounding!(
            (row, col, self.height, self.width), (dr, dc) -> {
                if self.look_is_occupied(row, col, dr, dc) { count += 1; }
            }
        );
        count
    }

    fn step_part2(&mut self) {
        self.grid = self.next.clone();
        for row in 0..self.height {
            for col in 0..self.width {
                let seat = self.grid[row][col];
                if seat == FLOOR {
                    continue;
                }

                let occ = self.count_visible_occupied(row, col);
                if seat == FREE && occ == 0 {
                    self.next[row][col] = OCCUPIED
                } else if seat == OCCUPIED && occ >= 5 {
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
        seats.step_part1();
        runs += 1;
    }
}

fn part2(mut seats: Seats) -> err::Result<u32> {
    let mut runs = 0;
    loop {
        // seats.display();
        if runs > 0 && seats.is_done() {
            return Ok(seats.count_occupied());
        }
        seats.step_part2();
        runs += 1;
    }
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
    let (ms, res) = time!(part2(input)?);
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
        assert_eq!(part1(input).expect("p1 fail"), 37);
    }

    #[test]
    fn test_p2() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part2(input).expect("p2 fail"), 26);
    }
}
