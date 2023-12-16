use crate::{utils, Result};

#[derive(Clone)]
struct Part {
    number: u64,
    is_part: bool,
    gear: Option<(usize, usize)>,
    #[allow(unused)]
    row: usize,
    #[allow(unused)]
    span: Vec<usize>,
    #[allow(unused)]
    adjacent: Vec<(usize, usize)>,
}
struct Grid {
    parts: Vec<Part>,
    #[allow(unused)]
    lines: Vec<Vec<char>>,
}

fn adjacent_to_span(span: &[usize], y: usize, max_x: usize, max_y: usize) -> Vec<(usize, usize)> {
    let mut res = vec![];
    let (first, last) = (span[0], span[span.len() - 1]);
    if first > 0 {
        res.push((first - 1, y));
    }
    if last < max_x {
        res.push((last + 1, y));
    }
    if y > 0 {
        if first > 0 {
            res.push((first - 1, y - 1));
        }
        if last < max_x {
            res.push((last + 1, y - 1));
        }
        for x in span {
            res.push((*x, y - 1));
        }
    }
    if y < max_y {
        if first > 0 {
            res.push((first - 1, y + 1));
        }
        if last < max_x {
            res.push((last + 1, y + 1));
        }
        for x in span {
            res.push((*x, y + 1));
        }
    }
    res
}

fn parse(s: &str) -> Result<Grid> {
    let lines = s
        .trim()
        .lines()
        .map(|s| s.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let mut parts = vec![];
    let end_y = lines.len() - 1;
    for (row, line) in lines.iter().enumerate() {
        let end_x = line.len() - 1;
        let mut word = String::new();
        let mut indices = vec![];
        for (i, c) in line.iter().enumerate() {
            if c.is_ascii_digit() {
                word.push(*c);
                indices.push(i);
            }
            if i == end_x || !c.is_ascii_digit() {
                if !word.is_empty() {
                    let number = word
                        .parse::<u64>()
                        .map_err(|e| format!("invalid number {e}"))?;
                    let span = indices.clone();
                    let adjacent = adjacent_to_span(&span, row, end_x, end_y);
                    let symbols = adjacent
                        .iter()
                        .filter_map(|(x, y)| {
                            let c = lines[*y][*x];
                            if c == '.' {
                                None
                            } else {
                                Some((c, (*x, *y)))
                            }
                        })
                        .collect::<Vec<_>>();
                    let is_part = !symbols.is_empty();
                    let gear = symbols
                        .iter()
                        .filter_map(|(c, coords)| if *c == '*' { Some(*coords) } else { None })
                        .next();
                    parts.push(Part {
                        number,
                        row,
                        span,
                        adjacent,
                        is_part,
                        gear,
                    });
                }
                word.clear();
                indices.clear();
            }
        }
    }
    Ok(Grid { parts, lines })
}

/// https://adventofcode.com/2023/day/3
fn part1(grid: &Grid) -> Result<u64> {
    let sum = grid
        .parts
        .iter()
        .filter_map(|p| if p.is_part { Some(p.number) } else { None })
        .sum();
    Ok(sum)
}

/// https://adventofcode.com/2023/day/3#part2
fn part2(grid: &Grid) -> Result<u64> {
    let mut gear_conn = map!();
    for part in grid.parts.iter().filter(|p| p.gear.is_some()) {
        let gear = part.gear.unwrap();
        let e = gear_conn.entry(gear).or_insert_with(std::vec::Vec::new);
        e.push(part.clone());
    }
    let sum = gear_conn
        .iter()
        .filter_map(|(_, parts)| {
            if parts.len() == 2 {
                Some(parts.iter().map(|p| p.number).product::<u64>())
            } else {
                None
            }
        })
        .sum();
    Ok(sum)
}

pub async fn run() -> Result<()> {
    let input = time!(
        utils::file::read("../input/d03.txt").await?,
        (ms) -> println!("  -> read[{ms:.3}ms]"),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{ms:.3}ms]"),
    );

    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{ms:.3}ms]: {res}");
    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{ms:.3}ms]: {res}");
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    static SAMPLE_INPUT: &str = r##"
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
    "##;

    #[tokio::test]
    async fn test_p1_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 4361);
    }

    #[tokio::test]
    async fn test_p1_real() {
        let input =
            parse(&utils::file::read("../input/d03.txt").await.unwrap()).expect("error parsing");
        let res = part1(&input).expect("error in part 1");
        assert_eq!(res, 550064);
    }

    #[tokio::test]
    async fn test_p2_sample() {
        let input = parse(SAMPLE_INPUT).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 467835);
    }

    #[tokio::test]
    async fn test_p2_real() {
        let input =
            parse(&utils::file::read("../input/d03.txt").await.unwrap()).expect("error parsing");
        let res = part2(&input).expect("error in part 2");
        assert_eq!(res, 85010461);
    }
}
