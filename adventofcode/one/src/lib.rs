#[macro_use]
extern crate log;

use std::collections::HashMap;


#[derive(Debug)]
enum Direction {
    North,
    South,
    East,
    West,
}
impl Direction {
    fn turn(&self, turn: &str) -> Direction {
        if turn == "R" {
            match *self {
                Direction::North => Direction::East,
                Direction::East => Direction::South,
                Direction::South => Direction::West,
                Direction::West => Direction::North,
            }
        } else {
            match *self {
                Direction::North => Direction::West,
                Direction::West => Direction::South,
                Direction::South => Direction::East,
                Direction::East => Direction::North,
            }
        }
    }
}

pub struct Position {
    x: i32,
    y: i32,
    dir: Direction,
}
impl Position {
    fn new(x: i32, y: i32) -> Position {
        Position {
            x: x,
            y: y,
            dir: Direction::North,
        }
    }
    fn travel(&mut self, directions: String, part2: bool) {
        // stuff for part2
        let mut memory_lane: HashMap<String, bool> = HashMap::new();
        memory_lane.insert("0,0".to_string(), true);

        // apply all directions
        for (turn, n) in directions.split(", ")
                                  .map(|pair| {
                                      let mut p = pair.trim().split("").skip(1);
                                      let turn = p.next().unwrap();
                                      let n = p.collect::<String>();
                                      (turn, n.parse::<i32>().unwrap())
                                  }) {
            let new_dir = self.dir.turn(turn);
            info!("turn: {:?}, new_dir: {:?}, n: {:?}", turn, new_dir, n);
            for _ in 0..n {
                let (x, y) = match new_dir {
                    Direction::North => (self.x, self.y + 1),
                    Direction::East => (self.x + 1, self.y),
                    Direction::South => (self.x, self.y - 1),
                    Direction::West => (self.x - 1, self.y),
                };
                info!(" ->> at: (x: {}, y: {})", x, y);
                self.x = x;
                self.y = y;
                if part2 {
                    let key = format!("{:?},{:?}", self.x, self.y);
                    let touched = memory_lane.entry(key).or_insert(false);
                    if *touched { return } else { *touched = true; }
                }
            }
            self.dir = new_dir;
        }
    }
    fn distance(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }
}

pub fn eval(prob: String, part2: bool) -> i32 {
    let mut pos = Position::new(0, 0);
    pos.travel(prob, part2);
    pos.distance()
}
