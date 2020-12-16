use crate::utils::err;
use crate::utils::file;

fn parse(input: &str) -> err::Result<Vec<usize>> {
    Ok(input
        .trim()
        .split(',')
        .map(|num| Ok(num.trim().parse::<usize>()?))
        .collect::<err::Result<Vec<_>>>()?)
}

fn solve(input: &[usize], end: usize) -> err::Result<usize> {
    let mut mem = vec![vec![0; 2]; end + 1];

    let mut turn = 1;
    let mut last_spoken = 0;
    for n in input {
        let v = mem.get_mut(*n).unwrap();
        v[0] = turn;
        turn += 1;
        last_spoken = *n;
    }

    while turn <= end {
        let speak = {
            let spoken_at = mem.get_mut(last_spoken).unwrap();
            if spoken_at[1] == 0 {
                0
            } else {
                spoken_at[0] - spoken_at[1]
            }
        };
        let spoken = mem.get_mut(speak).unwrap();
        spoken.swap(0, 1);
        spoken[0] = turn;
        last_spoken = speak;
        turn += 1;
    }
    Ok(last_spoken)
}

fn part1(input: &[usize]) -> err::Result<usize> {
    solve(input, 2020)
}

fn part2(input: &[usize]) -> err::Result<usize> {
    solve(input, 30000000)
}

pub fn run() -> err::Result<()> {
    let raw_input = time!(
        file::read("../input/d15.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );

    let input = time!(
        parse(&raw_input)?,
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

    static INPUT: &[(&str, usize)] = &[
        ("0,3,6", 436),
        ("1,3,2", 1),
        ("2,1,3", 10),
        ("1,2,3", 27),
        ("2,3,1", 78),
        ("3,2,1", 438),
        ("3,1,2", 1836),
    ];

    #[test]
    fn test_p1() {
        for &(input, ans) in INPUT {
            let input = parse(input).expect("parse fail");
            assert_eq!(part1(&input).expect("p1 fail"), ans);
        }
    }

    // static INPUT2: &[(&str, usize)] = &[
    //     ("0,3,6", 175594),
    //     ("1,3,2", 2578),
    //     ("2,1,3", 3544142),
    //     ("1,2,3", 261214),
    //     ("2,3,1", 6895259),
    //     ("3,2,1", 18),
    //     ("3,1,2", 362),
    // ];
    //
    // #[test]
    // fn test_p2() {
    //     for &(input, ans) in INPUT2 {
    //         let input = parse(input).expect("parse fail");
    //         assert_eq!(part2(&input).expect("p2 fail"), ans);
    //     }
    // }
}
