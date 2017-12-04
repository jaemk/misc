use {Dir};


#[derive(Debug, Clone)]
struct MemPoint {
    x: u32,
    y: u32,
}
impl MemPoint {
    fn step_distance(&self, other: &Self) -> u32 {
        let a = if self.x > other.x { self.x - other.x } else { other.x - self.x };
        let b = if self.y > other.y { self.y - other.y } else { other.y - self.y };
        a + b
    }
    fn step(&mut self, dir: &Dir) {
        match *dir {
            Dir::Up     => self.y += 1,
            Dir::Left   => self.x -= 1,
            Dir::Down   => self.y -= 1,
            Dir::Right  => self.x += 1,
        };
    }
}


struct MemTrack {
    size: u32,
}
impl MemTrack {
    fn find_port(&self, port: u32) -> MemPoint {
        let mut value = if self.size < 3 { 2 } else { (self.size - 2).pow(2) + 1 };
        let start_x = self.size;
        let start_y = if self.size == 1 { 1 } else { 2 };
        let mut point = MemPoint { x: start_x, y: start_y };
        let mut dir = Dir::Up;
        debug!("");
        debug!("Looking for: {}", port);
        debug!("square size: {}", self.size);
        while value < port {
            debug!("{:>4}: {:?}", value, point);
            match dir {
                Dir::Up => {
                    if point.y < self.size { point.step(&dir); }
                    else { dir = dir.turn(); continue }
                }
                Dir::Left => {
                    if point.x > 1 { point.step(&dir); }
                    else { dir = dir.turn(); continue }
                }
                Dir::Down => {
                    if point.y > 1 { point.step(&dir); }
                    else { dir = dir.turn(); continue }
                }
                Dir::Right => {
                    if point.y < self.size { point.step(&dir); }
                    else { dir = dir.turn(); continue }
                }
            }
            value += 1;
        }
        debug!("{:>4}: {:?}", value, point);
        point
    }
}


pub fn step_distance(n: u32) -> u32 {
    // Determine the size of the memory required to hold this number.
    // The size must be an odd number.
    let size = (n as f64).sqrt().ceil() as u32;
    let size = if size % 2 == 0 { size + 1 } else { size };
    let origin = if size == 1 { 1 } else { ((size - 1) / 2)  + 1 };
    let origin = MemPoint { x: origin, y: origin };
    debug!("origin: {:?}", origin);

    // x, y
    let mem = MemTrack { size };
    let point = mem.find_port(n);
    origin.step_distance(&point)
}

