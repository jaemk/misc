
static INPUT: &'static str = include_str!("../../input.txt");


type Result<T> = std::result::Result<T, Box<std::error::Error>>;


fn parse_jumps(input: &str) -> Result<Vec<i32>> {
    input.trim().lines()
        .map(|line| {
            let num = line.trim().parse::<i32>()?;
            Ok(num)
        }).collect()
}


mod p1 {
    use super::*;

    fn escape(jumps: &mut [i32]) -> usize {
        let mut count = 0;
        let mut index = 0i32;
        let size = jumps.len() as i32;
        while index >= 0 && index < size {
            let elem = jumps.get_mut(index as usize).expect("Already bounds checked");
            index += *elem;
            *elem += 1;
            count += 1;
        }
        count
    }

    pub fn solve(input: &str) -> usize {
        let mut jumps = parse_jumps(input).expect("Found invalid jump instruction");
        escape(&mut jumps)
    }
}


mod p2 {
    use super::*;

    fn escape(jumps: &mut [i32]) -> usize {
        let mut count = 0;
        let mut index = 0i32;
        let size = jumps.len() as i32;
        while index >= 0 && index < size {
            let elem = jumps.get_mut(index as usize).expect("Already bounds checked");
            index += *elem;
            if *elem >= 3 {
                *elem -= 1;
            } else {
                *elem += 1;
            }
            count += 1;
        }
        count
    }

    pub fn solve(input: &str) -> usize {
        let mut jumps = parse_jumps(input).expect("Found invalid jump instruction");
        escape(&mut jumps)
    }
}


pub fn main() {
    println!("d5-p1: {}", p1::solve(INPUT));
    println!("d5-p2: {}", p2::solve(INPUT));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        let input = "0\n3\n0\n1\n-3\n";
        let ans = p1::solve(input);
        assert_eq!(ans, 5, "Expected 5 steps to reach the exit");
    }

    #[test]
    fn p2() {
        let input = "0\n3\n0\n1\n-3\n";
        let ans = p2::solve(input);
        assert_eq!(ans, 10, "Expected 10 steps to reach the exit");
    }
}
