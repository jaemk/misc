/*!
http://adventofcode.com/2017/day/19
*/

static INPUT: &'static str = include_str!("../input.txt");


type Field = Vec<Vec<char>>;


struct Pos {
    x: usize,
    y: usize,
}
impl Pos {
    /// Find the starting position on the field. This will be on the
    /// second line since we've padded the field with whitespace
    fn find_start(field: &Field) -> Self {
        let (x, _) = field[1].iter().enumerate()
            .filter(|&(_, c)| *c == '|')
            .nth(0).unwrap();
        Self { x, y: 1 }
    }

    /// Calculate the next position in the given direction
    fn step(&self, dir: &Dir) -> Self {
        use Dir::*;
        match *dir {
            Up => Pos { x: self.x, y: self.y - 1 },
            Down => Pos { x: self.x, y: self.y + 1 },
            Left => Pos { x: self.x - 1, y: self.y },
            Right => Pos { x: self.x + 1, y: self.y },
        }
    }

    fn char(&self, field: &Field) -> char {
        field[self.y][self.x]
    }

    /// Figure out which way to turn and the next position after turning
    fn next_turn_pos(&self, field: &Field, current_dir: &Dir) -> (Dir, Pos) {
        use Dir::*;
        match *current_dir {
            Up | Down => {
                let left = self.step(&Dir::Left);
                if ! left.char(field).is_whitespace() {
                    return (Dir::Left, left)
                }
                let right = self.step(&Dir::Right);
                if ! right.char(field).is_whitespace() {
                    return (Dir::Right, right)
                }
            }
            Left | Right => {
                let up = self.step(&Dir::Up);
                if ! up.char(field).is_whitespace() {
                    return (Dir::Up, up)
                }
                let down = self.step(&Dir::Down);
                if ! down.char(field).is_whitespace() {
                    return (Dir::Down, down)
                }
            }
        }
        unreachable!();
    }
}


enum Dir {
    Up, Down, Left, Right,
}


fn stroll(input: &str) -> (usize, Vec<char>) {
    let mut field = input.lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    // pad the field with whitespace
    for line in &mut field {
        line.insert(0, ' ');
        line.push(' ');
    }
    let line_size = field[0].len();
    field.insert(0, vec![' '; line_size]);
    field.push(vec![' '; line_size]);

    let mut pos = Pos::find_start(&field);

    let mut dir = Dir::Down;
    let mut chars = vec![];
    let mut count = 0;
    loop {
        count += 1;
        pos = pos.step(&dir);
        let new_char = pos.char(&field);
        match new_char {
            '|' | '-' => continue,
            '+' => {
                count += 1;

                let (new_dir, new_pos) = pos.next_turn_pos(&field, &dir);
                pos = new_pos;
                dir = new_dir;

                let c = pos.char(&field);
                if c != '-' && c != '|' { chars.push(c); }
            }
            c => {
                if c.is_whitespace() { break; }
                chars.push(c);
            }
        }
    }
    (count, chars)
}


pub fn main() {
    let (count, chars) = stroll(INPUT);
    println!("d19-p1: {:?}", chars);
    println!("d19-p2: {:?}", count);
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &'static str = r##"     |          
     |  +--+    
     A  |  C    
 F---|--|-E---+ 
     |  |  |  D 
     +B-+  +--+ "##;

    #[test]
    fn p1() {
        let (_, chars) = stroll(TEST_INPUT);
        assert_eq!(vec!['A', 'B', 'C', 'D', 'E', 'F'], chars);
    }

    #[test]
    fn p2() {
        let (count, _) = stroll(TEST_INPUT);
        assert_eq!(38, count);
    }
}
