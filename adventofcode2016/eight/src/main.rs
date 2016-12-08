extern crate eight;

use std::io::Read;
use std::fs::File;


pub fn main() {
    println!("Advent of Code! Challenge 8");
    let mut file = File::open("input.txt").unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);

    let mut screen = eight::Screen::new(50, 6);
    let (n, display) = eight::eval(content, &mut screen);
    println!("Number of pixels lit [Part 1]: {}", n);
    println!("Screen display [Part 2] :\n{}", display);
}

#[cfg(test)]
mod tests {
    use super::eight;

    #[test]
    fn given() {
        let instr = r#"
rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1
"#.to_string();
        let res_screen = r#"-#--#-#
#-#----
-#-----"#.to_string();
        let mut screen = eight::Screen::new(7, 3);
        let (n, display) = eight::eval(instr, &mut screen);
        assert_eq!(n, 6);
        assert_eq!(res_screen, display.trim());
    }
}
