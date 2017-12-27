/// Calculate the number of digits in the number
/// for ints with more than the required 4 spaces
fn digit_size(n: u32) -> u32 {
    if n < 9999 {
        4
    } else {
        (n as f64).log(10.) as u32 + 1
    }
}

/// Return the largest digit (0-9) of a multi-digit integer
pub fn largest_digit(n: u32) -> u32 {
    let mut largest = 0;
    for i in 0..digit_size(n) {
        let div = 10u32.pow(i);
        let digit = (n % (10 * div)) / div;
        if digit > largest { largest = digit }
    }
    largest
}

/// For use with `order_digits`
pub enum DigitOrder {
    Asc,
    Desc,
}

/// Orders a number as either ascending or descending digits
/// 3241,   desc -> 4321
/// 324,    asc  -> 0234
/// 123456, desc -> 654321
pub fn order_digits(n: u32, order: DigitOrder) -> u32 {
    let n_digits = digit_size(n);
    let mut digits: Vec<u32> = Vec::with_capacity(n_digits as usize);
    for i in 0..n_digits {
        let div = 10u32.pow(i);
        let d = (n % (10 * div)) / div;
        let idx = match digits.binary_search(&d) {
            Ok(idx) => idx,
            Err(idx) => idx,
        };
        digits.insert(idx, d);
    }
    match order {
        DigitOrder::Desc => {
            digits.iter().enumerate().fold(0, |acc, (i, d)| {
                acc + d * 10u32.pow(i as u32)
            })}
        DigitOrder::Asc => {
            digits.iter().rev().enumerate().fold(0, |acc, (i, d)| {
                acc + d * 10u32.pow(i as u32)
            })}
    }
}

/// Order number with descending digits
pub fn desc_digits(n: u32) -> u32 {
    order_digits(n, DigitOrder::Desc)
}

/// Counts the number of (ascending - descending) operations
/// required to reach kaprekar's constant (6174)
pub fn kaprekar(n: u32) -> u32 {
    let mut count = 0;
    let mut num = n;
    let mut prev = n;
    loop {
        num = order_digits(num, DigitOrder::Desc) - order_digits(num, DigitOrder::Asc);
        if num == prev { return count; }
        prev = num;
        count += 1;
    }
}

