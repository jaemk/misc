extern crate hw1;

use hw1::problem2::sieve;
use hw1::problem3::hanoi;

pub fn main() {
    println!("{:?}", sieve(12));
    hanoi(5);

}
