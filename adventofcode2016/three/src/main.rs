extern crate three;

use std::io::Read;
use std::path::Path;
use std::fs::File;


pub fn main() {
    println!("Advent of Code! Challenge 3");

    let path = Path::new("input.txt");
    let mut file = File::open(&path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);

    let n = three::eval(&content, three::Part::One);
    println!("Total valid triangles [Part 1]: {}", n);

    let n = three::eval(&content, three::Part::Two);
    println!("Total valid triangles [Part 2]: {}", n);
}

