use std::collections::VecDeque;


pub struct Screen {
    field: Vec<VecDeque<bool>>,
    height: usize,
}
impl Screen {
    /// Return a new Screen with a field size of specified `width` and `height`
    pub fn new(width: usize, height: usize) -> Screen {
        let mut field = vec![VecDeque::with_capacity(width); height];
        for row in field.iter_mut() {
            for _ in 0..width { row.push_back(false); }
        }
        Screen {
            height: height,
            field: field,
        }
    }

    /// Turn on all pixels inside of the rectangle of size `width` x `height`
    /// in the top left corner or the screen.field
    fn add_rect(&mut self, width: usize, height: usize) {
        for row in 0..height {
            for col in 0..width {
                self.field[row][col] = true;
            }
        }
    }

    /// Rotate pixels of `row` right by `n` places, wrapping the right
    /// side pixels over to the left side
    fn rotate_row(&mut self, row: usize, n: u32) {
        for _ in 0..n {
            let c = self.field[row].pop_back().unwrap();
            self.field[row].insert(0, c);
        }
    }

    /// Rotate pixels of `col` down by `n` places, wrapping the bottom
    /// side pixels up to the top
    fn rotate_col(&mut self, col: usize, n: u32) {
        for _ in 0..n {
            let mut prev = self.field[self.height-1][col];
            for row in self.field.iter_mut() {
                let hold = row[col];
                row[col] = prev;
                prev = hold;
            }
        }
    }

    /// Return a formatted representation of the screen.field as a String
    fn repr(&self) -> String {
        self.field.iter().fold(String::new(), |mut out, line| {
            let c_line = line.iter().map(|&on| if on { '#' } else { '-' }).collect::<String>();
            out.push_str(&c_line);
            out.push('\n');
            out
        })
    }
}


pub fn eval(content: String, screen: &mut Screen) -> (u32, String) {
    for line in content.trim().split('\n') {
        let mut args = line.trim().split(' ');
        match args.next().unwrap() {
            "rect" => {
                let mut xy = args.next().unwrap().split('x');
                let width = xy.next().unwrap().parse::<usize>().unwrap();
                let height = xy.next().unwrap().parse::<usize>().unwrap();
                screen.add_rect(width, height);
            }
            "rotate" => {
                match args.next().unwrap() {
                    "row" => {
                        let row = args.next().unwrap().split('=').last().unwrap().parse::<usize>().unwrap();
                        let n = args.last().unwrap().parse::<u32>().unwrap();
                        screen.rotate_row(row, n);
                    }
                    "column" => {
                        let col = args.next().unwrap().split('=').last().unwrap().parse::<usize>().unwrap();
                        let n = args.last().unwrap().parse::<u32>().unwrap();
                        screen.rotate_col(col, n);
                    }
                    _ => (),
                }
            }
            _ => (),
        }
    }
    let n = screen.field.iter().fold(0, |acc, row| {
        acc + row.iter().fold(0, |acc, &item| {
            if item { acc + 1 } else { acc }
        })
    });
    (n, screen.repr())
}
