extern crate five;

use std::io::Read;
use std::path::Path;
use std::fs::File;


pub fn main() {
    println!("Advent of Code! Challenge 5");
    let p = Path::new("input.txt");
    let mut file = File::open(&p).unwrap();
    let mut input = String::new();
    let _ = file.read_to_string(&mut input);

    let password = five::eval(input.trim(), five::Part::One);
    println!("Door password [Part 1]: {}", password);

    let password = five::eval(input.trim(), five::Part::Two);
    println!("Door password [Part 2]: {}", password);
}
