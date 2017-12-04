use {Dir};


/// A Position on the spiraling ring
#[derive(Debug, Clone)]
pub struct Pos {
    x: usize,
    y: usize,
}
impl Pos {
    fn step(&self, dir: &Dir, field: &Vec<Vec<u32>>, pos: &Pos) -> (Dir, Self) {
        let left_value = match *dir {
            Dir::Up     => field[pos.x-1][pos.y],
            Dir::Left   => field[pos.x][pos.y-1],
            Dir::Down   => field[pos.x+1][pos.y],
            Dir::Right  => field[pos.x][pos.y+1],
        };
        let dir = if left_value == 0 {
            dir.turn()
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


/// An iterator over the spiraling memory field with recursively
/// growing values
pub struct Mem {
    _size: usize,
    max_count: usize,
    origin: usize,
    count: usize,
    dir: Dir,
    pos: Pos,
    field: Vec<Vec<u32>>,
}
impl Mem {
    pub fn with_size(size: usize) -> Self {
        if size % 2 == 0 { panic!("size must be odd") }
        let origin = if size == 1 { 1 } else { (size / 2) - 1 };
        let field = vec![vec![0; size]; size];
        Self {
            _size: size,
            max_count: size.pow(2),
            origin: origin,
            count: 0,
            dir: Dir::Right,
            pos: Pos { x: 0, y: 0 },
            field: field,
        }
    }

    fn count_surrounding(&self, pos: &Pos) -> u32 {
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
    type Item = (Pos, u32);
    fn next(&mut self) -> Option<Self::Item> {
        if self.count >= self.max_count { return None }
        let value = if self.count == 0 {
            self.count += 1;
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
        Some((self.pos.clone(), value))
    }
}

