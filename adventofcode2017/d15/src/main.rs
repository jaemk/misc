/*!
http://adventofcode.com/2017/day/15
*/

struct Generator {
    factor: u64,
    multiple_of: u64,
    div: u64,
    value: u64,
}
impl Generator {
    fn new(initial: u64, factor: u64, multiple_of: u64) -> Self {
        Self {
            factor,
            multiple_of,
            div: 2147483647,
            value: initial,
        }
    }
}
impl Iterator for Generator {
    type Item = u64;
    fn next(&mut self) -> Option<Self::Item> {
        let mut v = (self.factor * self.value) % self.div;
        let next = loop {
            if v % self.multiple_of == 0 { break v }
            v = (self.factor * v) % self.div;
        };
        self.value = next;
        Some(next)
    }
}


fn judge(a_start: u64, a_mult: u64, b_start: u64, b_mult: u64, iters: usize) -> u64 {
    let gen_a = Generator::new(a_start, 16807, a_mult);
    let gen_b = Generator::new(b_start, 48271, b_mult);
    gen_a.zip(gen_b)
        .take(iters)
        .fold(0, |sum, (a, b)| {
            if (0xFFFF & a) == (0xFFFF & b) {
                sum + 1
            } else {
                sum
            }
        })
}


fn part1(a_start: u64, b_start: u64) -> u64 {
    return judge(a_start, 1, b_start, 1, 40_000_000);
}


fn part2(a_start: u64, b_start: u64) -> u64 {
    return judge(a_start, 4, b_start, 8, 5_000_000);
}


pub fn main() {
    println!("d15-p1: {}", part1(512, 191));
    println!("d15-p2: {}", part2(512, 191));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        let count = part1(65, 8921);
        assert_eq!(count, 588);
    }

    #[test]
    fn p2() {
        let count = part2(65, 8921);
        assert_eq!(count, 309);
    }
}

