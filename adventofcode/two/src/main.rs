extern crate two;

use std::io::prelude::*;
use std::path::Path;
use std::fs::File;


pub fn main() {
    println!("Advent of code! Challenge 2");

    let p = Path::new("input.txt");
    let mut file = File::open(&p).expect("file error");
    let mut content = String::new();
    file.read_to_string(&mut content).expect("read error");

    let code = two::eval(&content, two::Part::One);
    println!("Bathroom code [Part 1]: {:?}", code);

    let code = two::eval(&content, two::Part::Two);
    println!("Bathroom code [Part 2]: {:?}", code);
}


#[cfg(test)]
mod test {
    use super::two;

    #[test]
    fn given() {
        let instr = "ULL\n\
                     RRDDD\n\
                     LURDL\n\
                     UUUUD".to_string();
        assert_eq!(two::eval(instr, two::Part::One), "1985");
    }
}
