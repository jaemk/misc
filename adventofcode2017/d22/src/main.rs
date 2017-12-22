
use std::collections::HashMap;


static INPUT: &'static str = include_str!("../input.txt");


type Error = Box<std::error::Error>;
type Result<T> = std::result::Result<T, Error>;


#[derive(Debug)]
enum Dir {
    Up, Down, Left, Right,
}
impl Dir {
    /// Turn, returning new `Dir`
    fn turn(&self, dir: &Dir) -> Self {
        use Dir::*;
        match *self {
            Up => match *dir {
                Left => Left,
                Right => Right,
                _ => panic!("Can't turn: {:?}!", dir),
            }
            Down => match *dir {
                Left => Right,
                Right => Left,
                _ => panic!("Can't turn: {:?}!", dir),
            }
            Left => match *dir {
                Left => Down,
                Right => Up,
                _ => panic!("Can't turn: {:?}!", dir),
            }
            Right => match *dir {
                Left => Up,
                Right => Down,
                _ => panic!("Can't turn: {:?}!", dir),
            }
        }
    }

    fn reverse(&self) -> Self {
        use Dir::*;
        match *self {
            Up => Down,
            Down => Up,
            Left => Right,
            Right => Left,
        }
    }
}


#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct Point {
    x: isize,
    y: isize,
}
impl Point {
    fn step(&self, dir: &Dir) -> Self {
        use Dir::*;
        let (x, y) = match *dir {
            Up      => (self.x, self.y-1),
            Down    => (self.x, self.y+1),
            Left    => (self.x-1, self.y),
            Right   => (self.x+1, self.y),
        };
        Self { x, y }
    }
}


#[derive(Debug)]
struct HashField {
    inner: HashMap<Point, Status>,
}
impl std::str::FromStr for HashField {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let grid = s.trim().lines().map(|line| {
            line.chars()
                .map(|c| if c == '#' { true } else { false } )
                .collect::<Vec<_>>()
        }).collect::<Vec<_>>();

        // grid must be odd sized
        if grid.len() % 2 == 0 { Err("Grid must be odd sized")? }
        if grid[0].len() % 2 == 0 { Err("Grid must be odd sized")? }

        let mut inner = HashMap::new();
        let mid = (grid.len() / 2) as isize;
        for (row_n, row) in grid.iter().enumerate() {
            for (col_n, elem) in row.iter().enumerate() {
                if *elem {
                    let point = Point {
                        x: col_n as isize - mid,
                        y: row_n as isize - mid,
                    };
                    inner.insert(point, Status::Infected);
                }
            }
        }
        Ok(Self { inner })
    }
}
impl HashField {
    fn status_at(&self, point: &Point) -> Status {
        self.inner.get(&point)
            .map(|status| status.clone())
            .unwrap_or(Status::Clean)
    }

    fn is_infected(&self, point: &Point) -> bool {
        self.status_at(point) == Status::Infected
    }

    /// Apply the next step of the infection to the given point
    fn step_infect(&mut self, point: Point) -> Status {
        use Status::*;
        let new_status = match self.status_at(&point) {
            Clean => Weakened,
            Weakened => Infected,
            Infected => Flagged,
            Flagged => Clean,
        };
        match new_status.clone() {
            Clean => self.inner.remove(&point),
            status => self.inner.insert(point, status),
        };
        new_status
    }

    fn infect(&mut self, point: Point) {
        self.inner.insert(point, Status::Infected);
    }

    fn clean(&mut self, point: &Point) {
        self.inner.remove(point);
    }
}


#[derive(Clone, Debug, Eq, PartialEq)]
enum Status {
    Clean,
    Weakened,
    Infected,
    Flagged,
}


struct Virus {
    loc: Point,
    dir: Dir,
}
impl Virus {
    fn new() -> Self {
        Self {
            loc: Point { x: 0, y: 0 },
            dir: Dir::Up,
        }
    }

    fn burst(&mut self, field: &mut HashField) -> Status {
        use Status::*;
        if field.is_infected(&self.loc) {
            self.dir = self.dir.turn(&Dir::Right);
            field.clean(&self.loc);
            self.loc = self.loc.step(&self.dir);
            Clean
        } else {
            self.dir = self.dir.turn(&Dir::Left);
            field.infect(self.loc.clone());
            self.loc = self.loc.step(&self.dir);
            Infected
        }
    }

    fn burst_evolved(&mut self, field: &mut HashField) -> Status {
        use Status::*;
        match field.status_at(&self.loc) {
            Clean => self.dir = self.dir.turn(&Dir::Left),
            Infected => self.dir = self.dir.turn(&Dir::Right),
            Flagged => self.dir = self.dir.reverse(),
            Weakened => (),
        };
        let new_status = field.step_infect(self.loc.clone());
        self.loc = self.loc.step(&self.dir);
        new_status
    }
}


fn part1(input: &str, iters: usize) -> Result<usize> {
    let mut field = input.parse::<HashField>()?;
    let mut virus = Virus::new();
    let mut infect_count = 0;
    for _ in 0..iters {
        match virus.burst(&mut field) {
            Status::Infected => infect_count += 1,
            _ => (),
        }
    }
    Ok(infect_count)
}


fn part2(input: &str, iters: usize) -> Result<usize> {
    let mut field = input.parse::<HashField>()?;
    let mut virus = Virus::new();
    let mut infect_count = 0;
    for _ in 0..iters {
        match virus.burst_evolved(&mut field) {
            Status::Infected => infect_count += 1,
            _ => (),
        }
    }
    Ok(infect_count)
}


fn time<F: Fn() -> Result<T>, T>(f: F) -> Result<(f64, T)> {
    use std::time;
    let start = time::Instant::now();
    let ans = f()?;
    let elap = start.elapsed();
    let ms = elap.as_secs() as f64 * 1000. + elap.subsec_nanos() as f64 / 1_000_000.;
    Ok((ms, ans))
}


fn run() -> Result<()> {
    let (ms, p1) = time(|| {
        let ans = part1(INPUT, 10_000)?;
        Ok(ans)
    })?;
    println!("d22-p1: [{:>12}ms] {}", ms, p1);

    let (ms, p2) = time(|| {
        let ans = part2(INPUT, 10_000_000)?;
        Ok(ans)
    })?;
    println!("d22-p2: [{:>12}ms] {}", ms, p2);
    Ok(())
}


pub fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &'static str = r##"
..#
#..
...
"##;

    #[test]
    fn p1_5_iters() {
        assert_eq!(5, part1(TEST_INPUT, 7).unwrap());
    }

    #[test]
    fn p1_10000_iters() {
        assert_eq!(5587, part1(TEST_INPUT, 10_000).unwrap());
    }

    #[test]
    fn p2_5_iters() {
        assert_eq!(26, part2(TEST_INPUT, 100).unwrap());
    }

    #[test]
    fn p2_10000000_iters() {
        assert_eq!(2511944, part2(TEST_INPUT, 10_000_000).unwrap());
    }
}

