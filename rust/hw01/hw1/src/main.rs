extern crate hw1;

use hw1::problem2::sieve;
use hw1::problem3::hanoi;
use hw1::problem4::{bloom, jenkins, fnv, djb2};

pub fn main() {
    println!("{:?}", sieve(12));
    hanoi(5);

    let data = vec!["apple", "blueberry", "carrot", "date", "eggplant",
        "fig", "grapefruit"];
    let hashes = [djb2, fnv, jenkins];
    bloom(&data, hashes, "carrot");
    bloom(&data, hashes, "milk");
    bloom(&data, hashes, "bread");
}
