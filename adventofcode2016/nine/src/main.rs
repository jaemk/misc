extern crate nine;

use std::io::Read;
use std::fs::File;


pub fn main() {
    println!("Advent of Code! Challenge 9");
    let mut file = File::open("input.txt").unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);

    let count = nine::decompress(&content);
    println!("Decompressed length [Part 1]: {}", count);

    let input = "(27x12)(20x12)(13x14)(7x10)(1x12)A";
    let count = nine::recursive_decompression(input);
    println!("Reverse Decompressed length [Part 2]: {}", count);
}

#[cfg(test)]
mod tests {
    use super::nine;
    #[test]
    fn given() {
        let input = "(27x12)(20x12)(13x14)(7x10)(1x12)A";
        assert_eq!(nine::recursive_decompression(input), 241920);
    }
}
