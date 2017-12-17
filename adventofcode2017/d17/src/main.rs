/*!
http://adventofcode.com/2017/day/17
*/

fn part1(step: usize) -> u32 {
    let mut buf = Vec::with_capacity(2017);
    buf.push(0);
    let mut ind = 0;
    for i in 1..2018 {
        ind = ((ind + step) % buf.len()) + 1;
        buf.insert(ind, i);
    }
    buf[ind+1]
}


/// Zero stays in the first position since things are
/// inserted after the next step
fn part2(step: usize) -> usize {
    let iters = 50_000_000;
    let mut ind = 0;
    let mut after_zero = 0;
    for i in 1..iters+1 {
        ind = ((ind + step) % i) + 1;
        if ind == 1 {
            after_zero = i;
        }
    }
    after_zero
}


pub fn main() {
    println!("d17-p1: {}", part1(335));
    println!("d17-p2: {}", part2(335));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        assert_eq!(part1(3), 638);
    }
}
