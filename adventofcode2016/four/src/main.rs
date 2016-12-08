extern crate four;

use std::io::Read;
use std::fs::File;
use std::path::Path;


pub fn main() {
    println!("Advent of Code! Challenge 4");
    let p = Path::new("input.txt");
    let mut file = File::open(&p).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);

    let sum = four::eval(&content, four::Part::One);
    println!("Sum of real-room section IDs [Part 1]: {}", sum);

    let id = four::eval(&content, four::Part::Two);
    println!("North Pole objects-store section ID [Part 2]: {}", id);
}

#[cfg(test)]
mod tests {
    use super::four;
    #[test]
    fn given() {
        println!("");
        println!("c st ofst rem");
        let encoded = "qzmt-zixmtkozy-ivhz";
        let n = 343;
        let msg = encoded.chars().map(|c| {
            if c == '-' { ' ' }
            else { four::cycle(c, n) }
        }).collect::<String>();
        assert_eq!("very encrypted name", msg);
    }
}
