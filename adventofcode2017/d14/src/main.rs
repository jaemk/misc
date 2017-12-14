/*!
http://adventofcode.com/2017/day/14
*/
extern crate d10;

use std::collections::HashSet;
use d10::knot;

static INPUT: &'static str = "nbysizxe";


type Field = Vec<Vec<bool>>;


/// Generate a 2-d hash field
fn hash_field(input: &str) -> Field {
    let mut rows = Vec::with_capacity(128);
    for i in 0..128 {
        let row_str = format!("{}-{}", input, i);
        let hash_bytes = knot::hash(&row_str);
        let mut row = Vec::with_capacity(128);
        for b in &hash_bytes {
            for flag in &[0b10000000, 0b1000000, 0b100000, 0b10000, 0b1000, 0b100, 0b10, 0b1] {
                row.push((b & flag) > 0);
            }
        }
        rows.push(row);
    }
    rows
}


fn part1(input: &str) -> usize {
    let field = hash_field(input);
    field.iter().fold(0, |sum, row| {
        sum + row.iter().fold(0, |row_sum, bit| {
            row_sum + if *bit { 1 } else { 0 }
        })
    })
}


mod part2 {
    use super::*;

    enum Dir {
        Up, Down, Left, Right,
    }

    /// A coordinate on the 2-d disk-frag field
    #[derive(Clone, Hash, Eq, PartialEq)]
    struct Coord {
        x: usize,
        y: usize,
    }
    impl Coord {
        /// Determine the next field coordinate in the given direction.
        /// Returns `None` if the coord is off the grid or is not "used"
        fn step(&self, dir: &Dir, field: &Field) -> Option<Self> {
            let (x, y) = match *dir {
                Dir::Up => {
                    if self.y > 0 {
                        (self.x, self.y - 1)
                    } else { return None }
                }
                Dir::Down => {
                    if self.y < field[0].len() - 1 {
                        (self.x, self.y + 1)
                    } else { return None }
                }
                Dir::Left => {
                    if self.x > 0 {
                        (self.x - 1, self.y)
                    } else { return None }
                }
                Dir::Right => {
                    if self.x < field.len() - 1 {
                        (self.x + 1, self.y)
                    } else { return None }
                }
            };
            if field[y][x] {
                Some(Self { x, y })
            } else {
                None
            }
        }
    }

    /// If the coord hasn't been seen before, save it and try stepping
    /// to adjacent coordinates.
    fn waltz(coord: Coord, field: &Field, group: &mut HashSet<Coord>) {
        if group.contains(&coord) { return }
        group.insert(coord.clone());

        use self::Dir::*;
        for dir in &[Up, Down, Left, Right] {
            if let Some(adj_coord) = coord.step(dir, field) {
                waltz(adj_coord, field, group);
            }
        }
    }

    pub fn solve(input: &str) -> usize {
        let field = hash_field(input);
        let mut groups: Vec<HashSet<Coord>> = vec![];
        for y in 0..field.len() {
            for x in 0..field[0].len() {
                let coord = Coord { x, y };
                if ! field[y][x] || groups.iter().any(|g| g.contains(&coord)) {
                    continue
                }
                let mut group = HashSet::new();
                waltz(coord, &field, &mut group);
                groups.push(group);
            }
        }
        groups.len()
    }
}


pub fn main() {
    println!("d14-p1: {}", part1(INPUT));
    println!("d14-p2: {}", part2::solve(INPUT));
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &'static str = "flqrgnkx";

    #[test]
    fn p1() {
        assert_eq!(part1(TEST_INPUT), 8108);
    }

    #[test]
    fn p2() {
        assert_eq!(part2::solve(TEST_INPUT), 1242);
    }
}

