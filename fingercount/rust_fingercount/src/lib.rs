
static VALS: [i32; 10] = [10, 10, 10, 10, 50, 5, 1, 1, 1, 1];

fn is_valid(fingers: &str) -> bool {
    if fingers.len() != 10 {
        return false
    }
    let f_chars = fingers.chars()
                         .map(|c| c.to_digit(10).unwrap_or(0))
                         .collect::<Vec<_>>();
    for i in (1..4).rev() {
        if f_chars[i-1] > f_chars[i] ||
           f_chars[10-i] > f_chars[10-i-1] {
            return false
        }
    }
    true
}

pub fn count(fingers: &str) -> Result<i32, &'static str> {
    if !is_valid(fingers) {
        return Err("invalid")
    }
    Ok(fingers.chars().enumerate()
           .fold(0, |acc, (ind, v)| {
                if v == '1' {
                    acc + VALS[ind]
                } else {
                    acc
                }
           }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn count_test1() {
        assert_eq!(count("0011001100"), Ok(22));
    }
    #[test]
    fn count_test2() {
        assert_eq!(count("0011101100"), Ok(72));
    }
    #[test]
    fn count_test3() {
        assert_eq!(count("0000111000"), Ok(56));
    }
    #[test]
    fn count_test4() {
        assert_eq!(count("0010101100"), Err("invalid"));
    }
    #[test]
    fn count_test5() {
        assert_eq!(count("0011101101"), Err("invalid"));
    }
}
