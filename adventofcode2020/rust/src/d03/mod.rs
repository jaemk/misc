use crate::utils::err;
use crate::utils::file;

const TREE: u8 = b'#';

struct Row<'a> {
    line: &'a [u8],
}
impl<'a> Row<'a> {
    #[inline]
    fn is_tree(&self, index: usize) -> bool {
        let size = self.line.len();
        let adjusted = index % size;
        let c = unsafe { *self.line.get_unchecked(adjusted) };
        c == TREE
    }
}

fn parse(input: &str) -> err::Result<Vec<Row>> {
    Ok(input
        .trim()
        .lines()
        .map(|line| Row {
            line: line.as_bytes(),
        })
        .collect::<Vec<_>>())
}

fn part1(input: &[Row]) -> err::Result<u64> {
    Ok(input
        .iter()
        .enumerate()
        .map(|(i, row)| {
            let index = i * 3;
            if row.is_tree(index) {
                1
            } else {
                0
            }
        })
        .sum())
}

const P2_MOVES: [(usize, usize); 5] = [
    // (right, down)
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2),
];

fn part2(input: &[Row]) -> err::Result<u64> {
    let end = input.len();
    Ok(P2_MOVES
        .iter()
        .map(|(right, down)| {
            let mut count = 0u64;
            let mut x = 0;
            let mut y = 0;
            while y < end {
                let row = unsafe { input.get_unchecked(y) };
                if row.is_tree(x) {
                    count += 1
                }

                x += right;
                y += down;
            }
            count
        })
        .product())
}

pub fn run() -> err::Result<()> {
    let input = file::read("../input/d03.txt")?;
    let input = parse(&input)?;

    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &str = r##"
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 7);
    }
    #[test]
    fn test_p2() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part2(&input).expect("p2 fail"), 336);
    }
}
