use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;

fn parse(input: &str) -> err::Result<Vec<u128>> {
    let mut jolts = input
        .trim()
        .lines()
        .map(|line| Ok(line.parse::<u128>()?))
        .collect::<err::Result<Vec<_>>>()?;
    jolts.push(0);
    jolts.sort_unstable();
    jolts.push(jolts[jolts.len() - 1] + 3);
    Ok(jolts)
}

fn part1(jolts: &[u128]) -> err::Result<u128> {
    let mut ones = 0;
    let mut threes = 0;
    for (a, b) in jolts.iter().tuple_windows() {
        let diff = b - a;
        if diff == 1 {
            ones += 1;
        } else if diff == 3 {
            threes += 1;
        }
    }
    Ok(ones * threes)
}

fn count_removal_permutations(jolts: &[u128]) -> u128 {
    fn remove(jolts: Vec<u128>, start: usize) -> u128 {
        let mut removals = 0;
        for (a, b, c) in (start..jolts.len()).tuple_windows() {
            if (jolts[c] - jolts[a]) <= 3 {
                let mut fork = jolts.clone();
                fork.remove(b);
                removals += 1 + remove(fork, a);
            }
        }
        removals
    }
    remove(jolts.to_vec(), 0) + 1
}

fn part2(jolts: &[u128]) -> err::Result<u128> {
    // collect groups that can have permutations within the full input
    let mut groups = vec![];
    let mut last_end = 0;
    let mut ptr = 1;
    while ptr < jolts.len() {
        if jolts[ptr] - jolts[ptr - 1] >= 3 {
            if ptr - last_end > 2 {
                // new permutation group
                let group = jolts[last_end..ptr].to_vec();
                groups.push(group);
            }
            last_end = ptr;
        }
        ptr += 1;
    }

    Ok(groups
        .iter()
        .map(|group| count_removal_permutations(group))
        .product())
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d10.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT_1: &str = r##"
16
10
15
5
1
11
7
19
6
12
4
"##;

    static INPUT_2: &str = r##"
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
"##;

    #[test]
    fn test_p1_1() {
        let input = parse(INPUT_1).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 35);
    }

    #[test]
    fn test_p1_2() {
        let input = parse(INPUT_2).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 220);
    }

    #[test]
    fn test_p2_1() {
        let input = parse(INPUT_1).expect("parse fail");
        assert_eq!(part2(&input).expect("p2 fail"), 8);
    }

    #[test]
    fn test_p2_2() {
        let input = parse(INPUT_2).expect("parse fail");
        assert_eq!(part2(&input).expect("p2 fail"), 19208);
    }
}
