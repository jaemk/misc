extern crate kaprekar;

pub fn main() {
    println!("{}", kaprekar::largest_digit(1234));
    println!("{}", kaprekar::order_digits(3241, kaprekar::DigitOrder::Desc));
    println!("{}", kaprekar::order_digits(241, kaprekar::DigitOrder::Desc));
    println!("{}", kaprekar::order_digits(3241, kaprekar::DigitOrder::Asc));
    println!("{}", kaprekar::kaprekar(6589));
    println!("{}", kaprekar::kaprekar(6174));
}
