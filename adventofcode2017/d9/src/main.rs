/*!
http://adventofcode.com/2017/day/9
*/

static INPUT: &'static str = include_str!("../input.txt");


/// Return stream (score, garbage-count)
fn process(input: &str) -> (u32, usize) {
    let mut stream = input.chars();
    let mut score = 0;
    let mut garbage = 0;
    let mut level = 0;
    while let Some(c) = stream.next() {
        match c {
            '<' => {
                loop {
                    let c = stream.next().expect("invalid input");
                    match c {
                        '!' => {
                            stream.next().expect("Invalid input");
                        }
                        '>' => break,
                        _ => garbage += 1,
                    }
                }
            }
            '{' => {
                level += 1;
                score += level;
            }
            '}' => {
                level -= 1;
            }
            _ => (),
        }
    }
    (score, garbage)
}


pub fn main() {
    let (score, garbage) = process(INPUT);
    println!("d9-p1: {}", score);
    println!("d9-p1: {}", garbage);
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn score_1() {
        assert_eq!(process("{}").0, 1);
    }

    #[test]
    fn score_2() {
        assert_eq!(process("{{{}}}").0, 6);
    }

    #[test]
    fn score_3() {
        assert_eq!(process("{{}, {}}").0, 5);
    }

    #[test]
    fn score_4() {
        assert_eq!(process("{{{},{},{{}}}}").0, 16);
    }

    #[test]
    fn score_5() {
        assert_eq!(process("{<a>,<a>,<a>,<a>}").0, 1);
    }

    #[test]
    fn score_6() {
        assert_eq!(process("{{<ab>},{<ab>},{<ab>},{<ab>}}").0, 9);
    }

    #[test]
    fn score_7() {
        assert_eq!(process("{{<!!>},{<!!>},{<!!>},{<!!>}}").0, 9);
    }

    #[test]
    fn score_8() {
        assert_eq!(process("{{<a!>},{<a!>},{<a!>},{<ab>}}").0, 3);
    }

    #[test]
    fn gc_1() {
        assert_eq!(process("<>").1, 0);
    }

    #[test]
    fn gc_2() {
        assert_eq!(process("<random characters>").1, 17);
    }

    #[test]
    fn gc_3() {
        assert_eq!(process("<<<<>").1, 3);
    }

    #[test]
    fn gc_4() {
        assert_eq!(process("<{!>}>").1, 2);
    }

    #[test]
    fn gc_5() {
        assert_eq!(process("<!!>").1, 0);
    }

    #[test]
    fn gc_6() {
        assert_eq!(process("<!!!>>").1, 0);
    }

    #[test]
    fn gc_7() {
        assert_eq!(process(r#"<{o"i!a,<{i<a>"#).1, 10);
    }
}
