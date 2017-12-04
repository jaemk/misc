/*!
http://adventofcode.com/2017/day/3
*/
#[macro_use] extern crate log;
extern crate env_logger;

mod part1;
mod part2;

use part1::step_distance;


const INPUT: u32 = 265149;


#[derive(Clone, Copy)]
enum Dir {
    Up,
    Down,
    Left,
    Right,
}
impl Dir {
    fn turn(&self) -> Self {
        match *self {
            Dir::Up     => Dir::Left,
            Dir::Left   => Dir::Down,
            Dir::Down   => Dir::Right,
            Dir::Right  => Dir::Up,
        }
    }
}


fn solve_part2(value: u32) -> u32 {
    let mem = part2::Mem::with_size(101);
    for (_, val) in mem {
        if val > value {
            return val;
        }
    }
    panic!("Failed solving part 2, got to value {}. Maybe increase Mem size?", value);
}


pub fn main() {
    init_logger().expect("log init error");

    println!("d3-p1: {}", step_distance(INPUT));
    println!("d3-p2: {}", solve_part2(INPUT));
}


fn init_logger() -> Result<(), Box<std::error::Error>> {
    // ::std::env::set_var("LOG", "info");
    env_logger::LogBuilder::new()
        .format(|record| {
            format!("[{}] - [{}] -> {}",
                record.level(),
                record.location().module_path(),
                record.args()
                )
            })
        .parse(&::std::env::var("LOG").unwrap_or_default())
        .init()?;
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        init_logger().expect("log init error");
        [(1, 0), (12, 3), (23, 2), (1024, 31)].iter()
            .for_each(|&(value, steps)| {
                assert_eq!(step_distance(value), steps, "Expected {} to be {} steps", value, steps);
            });
    }

    #[test]
    fn p2() {
        [(25, 26), (747, 806), (26, 54), (1, 2)].iter()
            .for_each(|&(value, expected)| {
                assert_eq!(solve_part2(value), expected, "Expected next largest {} for value {}", expected, value);
            })
    }
}
