/*!
http://adventofcode.com/2017/day/3
*/
#[macro_use] extern crate log;

pub mod part1;
pub mod part2;


#[derive(Clone, Copy)]
enum Dir {
    Up,
    Down,
    Left,
    Right,
}
impl Dir {
    fn turn_left(&self) -> Self {
        match *self {
            Dir::Up     => Dir::Left,
            Dir::Left   => Dir::Down,
            Dir::Down   => Dir::Right,
            Dir::Right  => Dir::Up,
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        [(1, 0), (12, 3), (23, 2), (1024, 31)].iter()
            .for_each(|&(value, steps)| {
                assert_eq!(part1::step_distance(value), steps, "Expected {} to be {} steps", value, steps);
            });
    }

    #[test]
    fn p2_vec() {
        [(25, 26), (747, 806), (26, 54), (1, 2)].iter()
            .for_each(|&(value, expected)| {
                assert_eq!(part2::vec::find_value_larger_than(value), expected,
                           "Expected next largest {} for value {}", expected, value);
            })
    }

    #[test]
    fn p2_hash() {
        [(25, 26), (747, 806), (26, 54), (1, 2)].iter()
            .for_each(|&(value, expected)| {
                assert_eq!(part2::hash::find_value_larger_than(value), expected,
                           "Expected next largest {} for value {}", expected, value);
            })
    }
}
