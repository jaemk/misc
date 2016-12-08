extern crate seven;

use std::fs::File;
use std::io::Read;


pub fn main() {
    println!("Advent of Code! Challenge 7");
    let mut file = File::open("input.txt").unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);

    let count = seven::eval(&content, seven::Part::One);
    println!("TLS supported [Part One]: {}", count);

    let count = seven::eval(&content, seven::Part::Two);
    println!("SSL supported [Part Two]: {}", count);
}
