#[macro_use]
extern crate log;
extern crate env_logger;
extern crate one;

use std::io::prelude::*;
use std::fs::File;
use std::path::Path;

pub fn main() {
    env_logger::init().unwrap();
    println!("Advent of code! Challenge 1");
    let p = Path::new("input.txt");
    let mut file = File::open(&p).expect("io error");
    let mut content = String::new();
    file.read_to_string(&mut content).expect("read error");
    debug!("{:?}", content);

    let dist = one::eval(&content, one::Part::One);
    println!("distance [Part 1]: {:?}", dist);

    let dist = one::eval(&content, one::Part::Two);
    println!("distance [Part 2]: {:?}", dist);
}

#[cfg(test)]
mod tests {
    use super::one;

    #[test]
    fn given() {
        assert_eq!(one::eval("R2, L3".to_string()), 5);
        assert_eq!(one::eval("R2, R2, R2".to_string()), 2);
        assert_eq!(one::eval("R5, L5, R5, R3".to_string()), 12);
    }
}
