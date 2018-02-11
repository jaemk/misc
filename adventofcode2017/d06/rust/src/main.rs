#[macro_use] extern crate log;
extern crate env_logger;

use std::collections::HashMap;


static INPUT: &'static str = include_str!("../../input.txt");


fn parse_input(input: &str) -> Result<Vec<usize>, Box<std::error::Error>> {
    input.trim()
        .split_whitespace()
        .map(|elem| Ok(elem.parse::<usize>()?))
        .collect()
}


/// Cycle the memory banks, redistributing the leftmost-largest memory bank
/// starting with the next to the right and wrapping around to the front.
fn cycle(banks: &mut [usize]) {
    let (max_index, max_value) = banks.iter().enumerate().fold((0, 0), |index_value, (i, value)| {
        if *value > index_value.1 { (i, *value) }
        else { index_value }
    });
    banks[max_index] = 0;
    let mut indices = (0..banks.len()).cycle().skip(max_index+1);
    for _ in 0..max_value {
        let index = indices.next().expect("cycled iterator shouldn't end");
        banks[index] += 1;
    }
}

/// Cycle memory banks until a repeating memory layout is found.
/// Return (total-count, count-between-repetition, memory-layout)
fn solve(input: &str) -> (usize, usize, Vec<usize>) {
    let mut banks = parse_input(input).expect("Failed parsing memory banks");
    debug!("{:>15}: {:?}", "banks_start", banks);
    let mut seen = HashMap::new();
    let mut count = 0;
    while !seen.contains_key(&banks) {
        seen.insert(banks.clone(), count);
        cycle(&mut banks);
        debug!("{:>15}: {:?}", "banks_cycled", banks);
        count += 1;
    }
    let last_seen_index = seen.get(&banks).expect("I know you're in there");
    (count, count - last_seen_index, banks)
}


pub fn main() {
    init_logger().expect("Failed initializing logger");
    let (count, diff, _banks) = solve(INPUT);
    println!("d6-p1: [count]: {}", count);
    println!("d6-p2: [diff]:  {}", diff)
}


/// Run with `LOG=debug cargo run` to see debug info
fn init_logger() -> Result<(), Box<std::error::Error>> {
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
        let bank = "0 2 7 0";
        let (count, _, _) = solve(bank);
        assert_eq!(count, 5, "Expected 5 steps to find repeating layout");
    }

    #[test]
    fn p2() {
        let bank = "0 2 7 0";
        let (_, diff, _) = solve(bank);
        assert_eq!(diff, 4, "Expected 4 cycles between repeating layouts");
    }
}

