use {Dir};


pub mod vec {
    use super::*;

    /// A Position on the spiraling ring
    #[derive(Debug, Clone)]
    pub struct Pos {
        x: usize,
        y: usize,
    }
    impl Pos {
        /// Step along the spiral, returning the new direction and x,y `Pos`
        fn step(&self, dir: &Dir, field: &Vec<Vec<u64>>, pos: &Pos) -> (Dir, Self) {
            let left_value = match *dir {
                Dir::Up     => field[pos.x-1][pos.y],
                Dir::Left   => field[pos.x][pos.y-1],
                Dir::Down   => field[pos.x+1][pos.y],
                Dir::Right  => field[pos.x][pos.y+1],
            };
            let dir = if left_value == 0 {
                dir.turn_left()
            } else { *dir };
            let mut pos = self.clone();
            match dir {
                Dir::Up     => pos.y += 1,
                Dir::Left   => pos.x -= 1,
                Dir::Down   => pos.y -= 1,
                Dir::Right  => pos.x += 1,
            };
            (dir, pos)
        }
    }


    /// An iterator over the spiraling memory field with recursively growing values
    pub struct Mem {
        size: usize,
        max_count: usize,
        origin: usize,
        count: usize,
        dir: Dir,
        pos: Pos,
        field: Vec<Vec<u64>>,
        expand_count: usize,
        max_expand_count: usize,
    }
    impl Mem {
        /// Initialize a square memory field with height & width of `size`
        /// Size must be odd so there can be an origin/center point.
        pub fn with_size(size: usize) -> Self {
            if size % 2 == 0 { panic!("size must be odd") }
            let size = size + 2;  // add padding so we don't overflow indices at boundaries
            let origin = if size == 1 { 1 } else { (size - 1) / 2 };
            let field = vec![vec![0; size]; size];
            Self {
                size: size,
                max_count: size.saturating_sub(2).pow(2) - 1,  // account for padding
                origin: origin,
                count: 0,
                dir: Dir::Down,
                pos: Pos { x: 0, y: 0 },
                field: field,
                expand_count: 0,
                max_expand_count: 50,
            }
        }

        fn expand(&mut self) {
            let new_size = (self.size * 2) + 3;
            debug!("Expanding. new size: {}", new_size);
            let increase = new_size - self.size;
            let side_pad = increase / 2;
            self.size = new_size;
            self.max_count = self.size.saturating_sub(2).pow(2) - 1;
            self.origin = (self.size - 1) / 2;
            self.pos = Pos { x: self.pos.x + side_pad, y: self.pos.y + side_pad };

            // pad field with zeros to grow to new size
            for row in self.field.iter_mut() {
                row.reserve(increase);
                for _ in 0..side_pad { row.insert(0, 0); }
                for _ in 0..side_pad { row.push(0); }
            }
            self.field.reserve(increase);
            for _ in 0..side_pad { self.field.insert(0, vec![0; new_size]); }
            for _ in 0..side_pad { self.field.push(vec![0; new_size]); }
            self.expand_count += 1;
        }

        /// Return the sum of the surrounding memory ports
        fn count_surrounding(&self, pos: &Pos) -> u64 {
            debug!("{:?}", self.field);
            debug!("surroundings[{:?}]: {:?}", pos,
                   [
                    self.field[pos.x-1][pos.y],
                    self.field[pos.x-1][pos.y-1],
                    self.field[pos.x-1][pos.y+1],
                    self.field[pos.x+1][pos.y],
                    self.field[pos.x+1][pos.y-1],
                    self.field[pos.x+1][pos.y+1],
                    self.field[pos.x][pos.y-1],
                    self.field[pos.x][pos.y+1],
                   ]);
            self.field[pos.x-1][pos.y] +
            self.field[pos.x-1][pos.y-1] +
            self.field[pos.x-1][pos.y+1] +

            self.field[pos.x+1][pos.y] +
            self.field[pos.x+1][pos.y-1] +
            self.field[pos.x+1][pos.y+1] +

            self.field[pos.x][pos.y-1] +
            self.field[pos.x][pos.y+1]
        }
    }


    impl Iterator for Mem {
        type Item = (Pos, u64);
        fn next(&mut self) -> Option<Self::Item> {
            if self.count >= self.max_count {
                if self.expand_count > self.max_expand_count {
                    return None
                }
                self.expand();
            }
            let value = if self.count == 0 {
                self.pos = Pos { x: self.origin, y: self.origin };
                self.field[self.origin][self.origin] = 1;
                1
            } else {
                let (next_dir, next_pos) = self.pos.step(&self.dir, &self.field, &self.pos);
                self.dir = next_dir;
                self.pos = next_pos;
                let next_value = self.count_surrounding(&self.pos);
                self.field[self.pos.x][self.pos.y] = next_value;
                next_value
            };
            self.count += 1;
            Some((self.pos.clone(), value))
        }
    }


    pub fn find_value_larger_than(value: u64) -> u64 {
        let mem = Mem::with_size(1);
        for (_point, val) in mem {
            if val > value {
                return val;
            }
        }
        panic!("Failed solving part2, got to value {}. Maybe increase Mem size?", value);
    }
}


pub mod hash {
    use super::*;
    use std::collections::HashMap;

    #[derive(PartialEq, Eq, Hash, Clone, Debug)]
    struct Point { x: isize, y: isize }
    impl Point {
        fn point_left(&self, dir: &Dir) -> Self {
            match *dir {
                Dir::Up     => Point { x: self.x-1, y: self.y },
                Dir::Left   => Point { x: self.x, y: self.y-1 },
                Dir::Down   => Point { x: self.x+1, y: self.y },
                Dir::Right  => Point { x: self.x, y: self.y+1 },
            }
        }
        fn point_forward(&self, dir: &Dir) -> Self {
            match *dir {
                Dir::Up     => Point { x: self.x, y: self.y+1 },
                Dir::Left   => Point { x: self.x-1, y: self.y },
                Dir::Down   => Point { x: self.x, y: self.y-1 },
                Dir::Right  => Point { x: self.x+1, y: self.y },
            }
        }
    }


    fn sum_surrounding(p: &Point, field: &HashMap<Point, u64>) -> u64 {
        field.get(&Point { x: p.x-1, y: p.y}).unwrap_or(&0) +
        field.get(&Point { x: p.x-1, y: p.y-1}).unwrap_or(&0) +
        field.get(&Point { x: p.x-1, y: p.y+1}).unwrap_or(&0) +

        field.get(&Point { x: p.x+1, y: p.y}).unwrap_or(&0) +
        field.get(&Point { x: p.x+1, y: p.y-1}).unwrap_or(&0) +
        field.get(&Point { x: p.x+1, y: p.y+1}).unwrap_or(&0) +

        field.get(&Point { x: p.x, y: p.y-1}).unwrap_or(&0) +
        field.get(&Point { x: p.x, y: p.y+1}).unwrap_or(&0)
    }


    pub fn find_value_larger_than(desired: u64) -> u64 {
        let mut seen = HashMap::new();
        let mut dir = Dir::Right;
        let mut point = Point { x: 0, y: 0 };
        seen.insert(point.clone(), 1);
        point = point.point_forward(&dir);

        loop {
            if seen.get(&point).is_none() {
                let value = sum_surrounding(&point, &seen);
                if value > desired { return value; }
                {
                    let e = seen.entry(point.clone()).or_insert(0);
                    *e = value;
                }
                continue
            }

            let left = point.point_left(&dir);
            if seen.get(&left).is_none() {
                dir = dir.turn_left();
                continue
            }

            point = point.point_forward(&dir);
        }
    }
}

