/*!
http://adventofcode.com/2017/day/2
*/
static INPUT: &'static str = include_str!("../../input.txt");

type Result<T> = std::result::Result<T, Box<std::error::Error>>;


fn read_sheet() -> Result<Vec<Vec<usize>>> {
    let mut sheet = vec![];
    for line in INPUT.trim().lines() {
        let mut row = vec![];
        for num in line.trim().split_whitespace() {
            let n = num.parse()?;
            row.push(n);
        }
        sheet.push(row);
    }
    Ok(sheet)
}


mod p1 {
    pub fn checksum_line(elems: &[usize]) -> usize {
        let mut min = usize::max_value();
        let mut max = usize::min_value();
        for e in elems {
            if *e < min { min = *e; }
            if *e > max { max = *e; }
        }
        max - min
    }

    pub fn checksum(sheet: &[Vec<usize>]) -> usize {
        sheet.iter().fold(0, |acc, row| acc + checksum_line(row.as_slice()))
    }
}


mod p2 {
    pub fn checksum_line(elems: &[usize]) -> usize {
        for e in elems {
            for e2 in elems {
                if e != e2 && e % e2 == 0 {
                    if e > e2 { return e / e2 }
                    else { return e2 / e }
                }
            }
        }
        unreachable!();
    }

    pub fn checksum(sheet: &[Vec<usize>]) -> usize {
        sheet.iter().fold(0, |acc, row| acc + checksum_line(row.as_slice()))
    }
}


fn run() -> Result<()> {
    let sheet = read_sheet()?;
    println!("d2-p1: {}", p1::checksum(sheet.as_slice()));
    println!("d2-p2: {}", p2::checksum(sheet.as_slice()));
    Ok(())
}


fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn part1() {
        [(vec![5, 1, 9, 5], 8), (vec![7, 5, 3], 4), (vec![2, 4, 6, 8], 6)].iter()
            .for_each(|&(ref row, sum)| {
                assert_eq!(p1::checksum_line(&row), sum, "row: {:?}, expected sum: {}", row, sum);
            });
    }

    #[test]
    fn part1_2() {
        let input = vec![
            vec![5, 1, 9, 5],
            vec![7, 5, 3],
            vec![2, 4, 6, 8],
        ];
        assert_eq!(p1::checksum(&input), 18);
    }

    #[test]
    fn part2() {
        [(vec![5, 9, 2, 8], 4), (vec![9, 4, 7, 3], 3), (vec![3, 8, 6, 5], 2)].iter()
            .for_each(|&(ref row, div)| {
                assert_eq!(p2::checksum_line(&row), div, "row: {:?}, expected div: {}", row, div);
            });
    }

    #[test]
    fn part2_2() {
        let input = vec![
            vec![5, 9, 2, 8],
            vec![9, 4, 7, 3],
            vec![3, 8, 6, 5],
        ];
        assert_eq!(p2::checksum(&input), 9);
    }
}
