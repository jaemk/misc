/*!
http://adventofcode.com/2017/day/15
*/
extern crate d15;


pub fn main() {
    println!("d15-p1: {}", d15::part1(512, 191));
    println!("d15-p1 (chan): {}", d15::part1_channels(10_000, 512, 191));

    println!("d15-p2: {}", d15::part2(512, 191));
    println!("d15-p2 (chan): {}", d15::part2_channels(10_000, 512, 191));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        let count = d15::part1(65, 8921);
        assert_eq!(count, 588);
    }

    #[test]
    fn p1_chan() {
        let count = d15::part1_channels(10_000, 65, 8921);
        assert_eq!(count, 588);
    }

    #[test]
    fn p2() {
        let count = d15::part2(65, 8921);
        assert_eq!(count, 309);
    }

    #[test]
    fn p2_chan() {
        let count = d15::part2_channels(10_000, 65, 8921);
        assert_eq!(count, 309);
    }
}

