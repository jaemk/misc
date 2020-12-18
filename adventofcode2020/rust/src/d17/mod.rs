use crate::utils::err;
use crate::utils::file;
use itertools::iproduct;
use itertools::Itertools;
use std::collections::HashSet;

mod part1 {
    use super::*;
    type Coord = (i64, i64, i64);

    #[derive(Debug, Clone)]
    pub struct Grid {
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

        fn offsets() -> Vec<Coord> {
            iproduct!(-1..=1, -1..=1, -1..=1)
                .filter_map(|(x, y, z)| {
                    let c = (x, y, z);
                    if c == (0, 0, 0) {
                        None
                    } else {
                        Some(c)
                    }
                })
                .collect_vec()
        }

        #[inline]
        fn count_set(&self, c: Coord, offsets: &[Coord], stop_if_gte: usize) -> usize {
            let mut count = 0;
            for off in offsets {
                let oc = (c.0 + off.0, c.1 + off.1, c.2 + off.2);
                if self.inner.contains(&oc) {
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

    pub fn parse(input: &str) -> err::Result<Grid> {
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

    pub fn solve(input: &Grid) -> err::Result<usize> {
        let offsets = Grid::offsets();
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

                let n_set = current.count_set((x, y, z), &offsets, stop_if_gte);
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
}

mod part2 {
    use super::*;

    type Coord = (i64, i64, i64, i64);

    #[derive(Debug, Clone)]
    pub struct Grid {
        // w, x, y, z
        inner: HashSet<Coord>,
    }
    impl Grid {
        fn clear(&mut self) {
            self.inner.clear();
        }

        fn active(&self) -> usize {
            self.inner.len()
        }

        fn offsets() -> Vec<Coord> {
            iproduct!(-1..=1, -1..=1, -1..=1, -1..=1)
                .filter_map(|(w, x, y, z)| {
                    let c = (w, x, y, z);
                    if c == (0, 0, 0, 0) {
                        None
                    } else {
                        Some(c)
                    }
                })
                .collect_vec()
        }

        #[inline]
        fn count_set(&self, c: Coord, offsets: &[Coord], stop_if_gte: usize) -> usize {
            let mut count = 0;
            for off in offsets {
                let oc = (c.0 + off.0, c.1 + off.1, c.2 + off.2, c.3 + off.3);
                if self.inner.contains(&oc) {
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
            let mut minw = 0;
            let mut maxw = 0;
            let mut minx = 0;
            let mut maxx = 0;
            let mut miny = 0;
            let mut maxy = 0;
            let mut minz = 0;
            let mut maxz = 0;
            for &(w, x, y, z) in self.inner.iter() {
                if w < minw {
                    minw = w;
                }
                if w > maxw {
                    maxw = w;
                }
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
            ((minw, minx, miny, minz), (maxw, maxx, maxy, maxz))
        }
    }

    pub fn parse(input: &str) -> err::Result<Grid> {
        const Z: i64 = 0;
        const W: i64 = 0;
        let mut inner = set!();
        for (y, line) in input.trim().lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                if c == '#' {
                    inner.insert((W, x as i64, y as i64, Z));
                }
            }
        }
        Ok(Grid { inner })
    }

    pub fn solve(input: &Grid) -> err::Result<usize> {
        let offsets = Grid::offsets();

        let mut current = input.clone();
        let mut next = input.clone();

        for _ in 0..6 {
            std::mem::swap(&mut current, &mut next);
            next.clear();
            let ((minw, minx, miny, minz), (maxw, maxx, maxy, maxz)) = current.min_max();
            for (w, x, y, z) in iproduct!(
                (minw - 2..maxw + 2),
                (minx - 2..maxx + 2),
                (miny - 2..maxy + 2),
                (minz - 2..maxz + 2)
            ) {
                let is_set = current.is_set((w, x, y, z));
                let stop_if_gte = 4;

                let n_set = current.count_set((w, x, y, z), &offsets, stop_if_gte);
                if is_set && (n_set == 2 || n_set == 3) {
                    next.toggle_on((w, x, y, z));
                } else if is_set {
                    next.toggle_off((w, x, y, z));
                } else if !is_set && n_set == 3 {
                    next.toggle_on((w, x, y, z));
                }
            }
        }
        Ok(next.active())
    }
}

pub fn run() -> err::Result<()> {
    let raw_input = time!(
        file::read("../input/d17.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );

    let input = time!(
        part1::parse(&raw_input)?,
        (ms) -> println!("  -> parse[1][{}ms]", ms),
    );

    let (ms, res) = time!(part1::solve(&input)?);
    println!("  -> p1[{}ms]: {}", ms, res);

    let input = time!(
        part2::parse(&raw_input)?,
        (ms) -> println!("  -> parse[2][{}ms]", ms),
    );

    let (ms, res) = time!(part2::solve(&input)?);
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
        let input = part1::parse(INPUT).expect("parse fail");
        assert_eq!(part1::solve(&input).expect("p1 fail"), 112);
    }

    #[test]
    fn test_p2() {
        let input = part2::parse(INPUT).expect("parse fail");
        assert_eq!(part2::solve(&input).expect("p2 fail"), 848);
    }
}
