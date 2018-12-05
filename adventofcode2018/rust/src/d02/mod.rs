use super::utils::{self, StdResult};


fn part_1(input: &str) -> StdResult<u32> {
    let mut twos = 0;
    let mut threes = 0;
    for line in input.lines() {
        let freqs = utils::freqs(line.chars());

        let mut has2 = false;
        let mut has3 = false;
        for (_, &v) in freqs.iter() {
            if v == 2 { has2 = true; }
            else if v == 3 { has3 = true; }

            if has2 && has3 { break; }
        }
        if has2 { twos += 1; }
        if has3 { threes += 1; }
    }
    Ok(twos * threes)
}


fn part_2(input: &str) -> StdResult<String> {
    let rows = input.lines().map(|s| s.chars().collect::<Vec<_>>()).collect::<Vec<_>>();

    for (i, row) in rows.iter().enumerate() {
        for other in &rows[i..] {
            let filtered = row.iter().zip(other.iter()).map(|(a, b)| {
                if a == b { Some(a) } else { None }
            }).filter_map(|a| a).collect::<String>();

            if filtered.len() == row.len() - 1 {
                return Ok(filtered)
            }
        }
    }
    unreachable!()
}


pub fn run() -> StdResult<()> {
    info!("*** Day 2 ***");

    let input = input_file!("d02.txt");

    let (ms1, p1) = time!({
        part_1(&input)?
    });
    info!("p1: {:?}", p1);

    let (ms2, p2) = time!({
        part_2(&input)?
    });
    info!("p2: {:?}", p2);

    info!("[Day 2 runtimes] p1: {}ms, p2: {}ms\n", ms1, ms2);
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let input = "abcdef\n\
                     bababc\n\
                     abbcde\n\
                     abcccd\n\
                     aabcdd\n\
                     abcdee\n\
                     ababab";
        let res = part_1(&input).unwrap();
        assert_eq!(res, 12);
    }

    #[test]
    fn test_part_2() {
        let input = "abcde\n\
                     fghij\n\
                     klmno\n\
                     pqrst\n\
                     fguij\n\
                     axcye\n\
                     wvxyz";
        let res = part_2(&input).unwrap();
        assert_eq!(res, "fgij");
    }
}
