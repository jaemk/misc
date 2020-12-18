use crate::utils::err;
use crate::utils::file;
use itertools::iproduct;
use std::collections::HashSet;

type Coord = (i64, i64, i64);

#[derive(Debug, Clone)]
struct Grid {
    // x, y, z
    inner: HashSet<Coord>,
}
impl Grid {
    fn clear(&mut self) {
        self.inner.clear();
    }

    fn active(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    fn surrounding(coord: Coord) -> [Coord; 26] {
        let (x, y, z) = coord;

        [
            // same plane
            (x, y + 1, z),
            (x, y - 1, z),
            (x + 1, y, z),
            (x - 1, y, z),
            (x - 1, y - 1, z),
            (x - 1, y + 1, z),
            (x + 1, y - 1, z),
            (x + 1, y + 1, z),
            // above
            (x, y, z + 1),
            (x, y + 1, z + 1),
            (x, y - 1, z + 1),
            (x + 1, y, z + 1),
            (x - 1, y, z + 1),
            (x - 1, y - 1, z + 1),
            (x - 1, y + 1, z + 1),
            (x + 1, y - 1, z + 1),
            (x + 1, y + 1, z + 1),
            // below
            (x, y, z - 1),
            (x, y + 1, z - 1),
            (x, y - 1, z - 1),
            (x + 1, y, z - 1),
            (x - 1, y, z - 1),
            (x - 1, y - 1, z - 1),
            (x - 1, y + 1, z - 1),
            (x + 1, y - 1, z - 1),
            (x + 1, y + 1, z - 1),
        ]
    }

    #[inline]
    fn count_set(&self, coords: &[Coord], stop_if_gte: usize) -> usize {
        let mut count = 0;
        for coord in coords {
            if self.inner.contains(coord) {
                count += 1;
            }
            if count >= stop_if_gte {
                break;
            }
        }
        count
    }

    #[inline]
    fn is_set(&self, coord: Coord) -> bool {
        self.inner.contains(&coord)
    }

    #[inline]
    fn toggle_on(&mut self, coord: Coord) {
        self.inner.insert(coord);
    }

    #[inline]
    fn toggle_off(&mut self, coord: Coord) {
        self.inner.remove(&coord);
    }

    #[inline]
    fn min_max(&self) -> (Coord, Coord) {
        let mut minx = 0;
        let mut maxx = 0;
        let mut miny = 0;
        let mut maxy = 0;
        let mut minz = 0;
        let mut maxz = 0;
        for &(x, y, z) in self.inner.iter() {
            if x < minx {
                minx = x;
            }
            if x > maxx {
                maxx = x;
            }
            if y < miny {
                miny = y;
            }
            if y > maxy {
                maxy = y;
            }
            if z < minz {
                minz = z;
            }
            if z > maxz {
                maxz = z;
            }
        }
        ((minx, miny, minz), (maxx, maxy, maxz))
    }
}

fn parse(input: &str) -> err::Result<Grid> {
    const Z: i64 = 0;
    let mut inner = set!();
    for (y, line) in input.trim().lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                inner.insert((x as i64, y as i64, Z));
            }
        }
    }
    Ok(Grid { inner })
}

fn part1(input: &Grid) -> err::Result<usize> {
    let mut current = input.clone();
    let mut next = input.clone();

    for _ in 0..6 {
        std::mem::swap(&mut current, &mut next);
        next.clear();
        let ((minx, miny, minz), (maxx, maxy, maxz)) = current.min_max();
        for (x, y, z) in iproduct!(
            (minx - 2..maxx + 2),
            (miny - 2..maxy + 2),
            (minz - 2..maxz + 2)
        ) {
            let is_set = current.is_set((x, y, z));
            let stop_if_gte = 4;

            let surr = Grid::surrounding((x, y, z));
            let n_set = current.count_set(&surr, stop_if_gte);
            if is_set && (n_set == 2 || n_set == 3) {
                next.toggle_on((x, y, z));
            } else if is_set {
                next.toggle_off((x, y, z));
            } else if !is_set && n_set == 3 {
                next.toggle_on((x, y, z));
            }
        }
    }
    Ok(next.active())
}

fn part2(input: &Grid) -> err::Result<u32> {
    Ok(1)
}

pub fn run() -> err::Result<()> {
    let raw_input = time!(
        file::read("../input/d17.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );

    let mut input = time!(
        parse(&raw_input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&mut input)?);
    println!("  -> p1[{}ms]: {}", ms, res);

    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &str = r##"
.#.
..#
###
"##;

    #[test]
    fn test_p1() {
        let mut input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&mut input).expect("p1 fail"), 112);
    }

    // #[test]
    // fn test_p2() {
    //     let mut input = parse(INPUT2).expect("parse fail");
    //     assert_eq!(part1(&mut input).expect("p1 fail"), 0);
    //     assert_eq!(part2(&input).expect("p2 fail"), 12 * 13);
    // }
}
