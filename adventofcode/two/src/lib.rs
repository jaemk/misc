type NumPad = [[char; 3]; 3];

const PAD: NumPad = [
    ['1', '2', '3'],
    ['4', '5', '6'],
    ['7', '8', '9'],
];


#[derive(Debug)]
struct Position {
    x: usize,
    y: usize,
}
impl Position {
    fn new(x: usize, y: usize) -> Position {
        Position { x: x, y: y}
    }

    fn move_dir(&mut self, dir: char) {
        match dir {
            'U' => if self.y > 0 { self.y -= 1; },
            'D' => if self.y < 2 { self.y += 1; },
            'L' => if self.x > 0 { self.x -= 1; },
            'R' => if self.x < 2 { self.x += 1; },
            _ => ()
        };
    }

    fn current_num(&self, pad: NumPad) -> char {
        pad[self.y][self.x]
    }
}


pub fn eval(directions: String) -> String {
    let mut pos = Position::new(1, 1);
    let mut code = String::new();
    for line in directions.trim().split('\n') {
        for c in line.chars() {
            pos.move_dir(c);
        }
        code.push(pos.current_num(PAD));
    }
    code
}
