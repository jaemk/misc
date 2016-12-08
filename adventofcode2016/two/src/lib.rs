pub enum Part {
    One,
    Two,
}


struct NumPad {
    field: Vec<Vec<char>>,
}
impl NumPad {
    fn valid(&self, x: usize, y: usize) -> bool {
        self.field[y][x] != '-'
    }
}


struct Position {
    x: usize,
    y: usize,
}
impl Position {
    fn new(x: usize, y: usize) -> Position {
        Position { x: x, y: y}
    }

    fn move_dir(&mut self, dir: char, pad: &NumPad) {
        let (x, y) = match dir {
            'U' => (self.x, self.y - 1),
            'D' => (self.x, self.y + 1),
            'L' => (self.x - 1, self.y),
            'R' => (self.x + 1, self.y),
            _ => (self.x, self.y),
        };
        if pad.valid(x, y) {
            self.x = x;
            self.y = y;
        }
    }

    fn current_num(&self, pad: &NumPad) -> char {
        pad.field[self.y][self.x]
    }
}


pub fn eval(directions: &String, part: Part) -> String {
    // setup numpad based on part1/2
    let field = match part {
        Part::One => vec![
                vec!['-', '-', '-', '-', '-'],
                vec!['-', '1', '2', '3', '-'],
                vec!['-', '4', '5', '6', '-'],
                vec!['-', '7', '8', '9', '-'],
                vec!['-', '-', '-', '-', '-'],
            ],
        Part::Two => vec![
            vec!['-', '-', '-', '-', '-', '-', '-'],
            vec!['-', '-', '-', '1', '-', '-', '-'],
            vec!['-', '-', '2', '3', '4', '-', '-'],
            vec!['-', '5', '6', '7', '8', '9', '-'],
            vec!['-', '-', 'A', 'B', 'C', '-', '-'],
            vec!['-', '-', '-', 'D', '-', '-', '-'],
            vec!['-', '-', '-', '-', '-', '-', '-'],
            ],
    };
    let pad = NumPad { field: field };

    // setup starting Position based on part1/2
    let mut pos = match part {
        Part::One => Position::new(2, 2),
        Part::Two => Position::new(1, 3),
    };

    let mut code = String::new();
    for line in directions.trim().split('\n') {
        for c in line.chars() {
            pos.move_dir(c, &pad);
        }
        code.push(pos.current_num(&pad));
    }
    code
}
