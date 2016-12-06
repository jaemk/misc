extern crate six;

use std::io::Read;
use std::fs::File;

pub fn main() {
    println!("Advent of Code! Challenge 6");

    let mut file = File::open("input.txt").unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);

    let msg = six::eval(&content, six::Part::One);
    println!("Secret message [Part 1]: {}", msg);

    let msg = six::eval(&content, six::Part::Two);
    println!("Secret message [Part 2]: {}", msg);
}

#[cfg(test)]
mod tests {
    use super::six;
    #[test]
    fn given() {
        let input = r#"
eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"#.to_string();
        assert_eq!(six::eval(&input, six::Part::One), "easter");
        assert_eq!(six::eval(&input, six::Part::Two), "advent");
    }
}
