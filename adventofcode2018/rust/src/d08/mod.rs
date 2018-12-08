use crate::utils::{StdResult};


fn parse(s: &str) -> Vec<usize> {
    s.trim()
        .split_whitespace()
        .map(|s| s.parse::<usize>().expect("invalid usize"))
        .collect()
}


fn part_1(input: &[usize]) -> usize {
    /// return (count, next-ind-start)
    fn inner(slice: &[usize]) -> (usize, usize) {
        let mut next_ind = 2;
        let mut count = 0;

        let n_children = slice[0];
        let n_meta = slice[1];

        for _ in 0..n_children {
            let (add_count, ind_offset) = inner(&slice[next_ind..]);
            count += add_count;
            next_ind += ind_offset;
        }

        for e in &slice[next_ind..(next_ind + n_meta)] {
            count += *e;
        }

        next_ind += n_meta;
        (count, next_ind)
    }

    let (sum, _) = inner(input);
    sum
}


fn part_2(input: &[usize]) -> usize {
    /// return (node-value, next-ind-start)
    fn inner(slice: &[usize], index: usize) -> (usize, usize) {
        let mut next_ind = 2;

        let n_children = slice[0];
        let n_meta = slice[1];

        let mut children = vec![];
        for _ in 0..n_children {
            let (child_val, ind_offset) = inner(&slice[next_ind..], index + 1);
            children.push(child_val);
            next_ind += ind_offset;
        }

        let value = if children.is_empty() {
            slice[next_ind..(next_ind + n_meta)].iter().sum()
        }
        else {
            slice[next_ind..(next_ind + n_meta)]
                .iter()
                .filter_map(|child_ind| children.get(*child_ind - 1))
                .sum()
        };

        next_ind += n_meta;
        (value, next_ind)
    }

    let (val, _) = inner(input, 0);
    val
}


pub fn run() -> StdResult<()> {
    info!("*** Day 8 ***");
    let (ms_parse, input) = time!({ parse(input_file!("d08.txt")) });

    let (ms1, res1) = time!({ part_1(&input) });
    info!("p1: {}", res1);

    let (ms2, res2) = time!({ part_2(&input) });
    info!("p2: {}", res2);

    info!("[Day 8 runtimes] p1: {}ms, p2: {}ms, parsing: {}ms\n",
          ms1, ms2, ms_parse);
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: &'static str = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2";

    #[test]
    fn test_part_1() {
        let nums = parse(INPUT);
        let res = part_1(&nums);
        assert_eq!(res, 138);
    }

    #[test]
    fn test_part_2() {
        let nums = parse(INPUT);
        let res = part_2(&nums);
        assert_eq!(res, 66);
    }
}
