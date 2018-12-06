use utils::{self, StdResult};


/// try to fold together (remove) the current index and index to the right
fn try_fold_right(ind: usize, buf: &mut Vec<char>) -> Option<usize> {
    let size = buf.len();
    let ind = if ind < size - 1 { ind } else { size - 2 };
    let right = ind + 1;
    let complement = (buf[ind] != buf[right]) &&
                           (buf[ind].to_ascii_lowercase() == buf[right].to_ascii_lowercase());
    if complement {
        buf.remove(right);
        buf.remove(ind);
    }

    if ind > 0 { Some(ind - 1) } else { None }
}


/// try folding elements together from right to left
fn reduce(input: &str) -> StdResult<String> {
    let mut buf = input.trim().chars().collect::<Vec<_>>();
    let size = buf.len();
    if size < 2 { return Ok(input.into()) }

    let mut ind = Some(size - 2);
    loop {
        if ind.is_none() { return Ok(buf.iter().collect()) }
        ind = try_fold_right(ind.unwrap(), &mut buf);
    }
}


/// size of reduced input
fn part_1(input: &str) -> StdResult<usize> {
    Ok(reduce(input)?.len())
}


/// smallest size of the reduced input after removing one of the present chars
fn part_2(input: &str) -> StdResult<(char, usize)> {
    let (min_char, min_size) = utils::freqs(input.trim().chars().map(|c| c.to_ascii_lowercase()))
        .iter()
        .map(|(c, _)| -> StdResult<(char, usize)> {
            let lower = c.to_ascii_lowercase().to_string();
            let upper = c.to_ascii_uppercase().to_string();
            let src = input.replace(&lower, "").replace(&upper, "");
            let reduced = reduce(&src)?;
            Ok((*c, reduced.len()))
        })
        .filter_map(Result::ok)
        .min_by_key(|(_c, size)| *size)
        .ok_or_else(|| "no min found")?;
    Ok((min_char, min_size))
}


pub fn run() -> StdResult<()> {
    info!("*** Day 5 ***");

    let input = input_file!("d05.txt");
    let (ms1, res1) = time!({
        part_1(input)?
    });
    info!("p1: {}", res1);

    let (ms2, (_char, res2)) = time!({
        part_2(input)?
    });
    info!("p2: {}", res2);

    info!("[Day 5 runtimes] p1: {}ms, p2: {}ms\n", ms1, ms2);
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        [
            ("aA", ""),
            ("abBA", ""),
            ("abAB", "abAB"),
            ("aabAAB", "aabAAB"),
            ("axfzZFXA", ""),
            ("dabAcCaCBAcCcaDA", "dabCBAcaDA")
        ].iter().for_each(|(input, exp)| {
            let res = reduce(input).unwrap();
            assert_eq!(&res, exp);
        });
    }

    #[test]
    fn test_part_2() {
        let (c, size) = part_2("dabAcCaCBAcCcaDA").unwrap();
        assert_eq!(c, 'c');
        assert_eq!(size, 4);
    }
}
