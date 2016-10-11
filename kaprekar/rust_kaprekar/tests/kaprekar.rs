extern crate kaprekar;

#[test]
fn working() {
    assert_eq!(0, 0);
}

#[test]
fn test_largest_digit() {
    assert_eq!(kaprekar::largest_digit(1234), 4);
    assert_eq!(kaprekar::largest_digit(3253), 5);
    assert_eq!(kaprekar::largest_digit(9800), 9);
    assert_eq!(kaprekar::largest_digit(3333), 3);
    assert_eq!(kaprekar::largest_digit(120), 2);
}

#[test]
fn test_desc_digits() {
    assert_eq!(kaprekar::desc_digits(1234), 4321);
    assert_eq!(kaprekar::desc_digits(3253), 5332);
    assert_eq!(kaprekar::desc_digits(9800), 9800);
    assert_eq!(kaprekar::desc_digits(3333), 3333);
    assert_eq!(kaprekar::desc_digits(120), 2100);
}

#[test]
fn test_kaprekar() {
    assert_eq!(kaprekar::kaprekar(6589), 2);
    assert_eq!(kaprekar::kaprekar(5455), 5);
    assert_eq!(kaprekar::kaprekar(6174), 0);
}
