use std::collections::HashMap;

static INPUT: &'static str = include_str!("../input.txt");


type AnyError = Box<std::error::Error>;
type Result<T> = std::result::Result<T, AnyError>;


#[derive(Debug)]
enum Move {
    Spin { n: usize },
    Exchange { ind_a: usize, ind_b: usize },
    Partner { a: char, b: char },
}
impl std::str::FromStr for Move {
    type Err = AnyError;
    fn from_str(op: &str) -> Result<Self> {
        use Move::*;
        Ok(if op.starts_with("s") {
            let n = op.trim_left_matches("s").parse::<usize>()?;
            Spin { n }
        } else if op.starts_with("p") {
            let mut parts = op.chars().skip(1).filter(|&c| c != '/');
            let a = parts.next()
                .ok_or_else(|| "Not enough instructions, Partner-a")?;
            let b = parts.next()
                .ok_or_else(|| "Not enough instructions, Partner-b")?;
            Partner { a, b }
        } else if op.starts_with("x") {
            let mut parts = op.trim_left_matches("x").split("/");
            let ind_a = parts.next()
                .ok_or_else(|| "Not enough instructions, Exchange-a")?
                .parse::<usize>()?;
            let ind_b = parts.next()
                .ok_or_else(|| "Not enough instructions, Exchange-b")?
                .parse::<usize>()?;
            Exchange { ind_a, ind_b }
        } else {
            Err("Invalid instructions")?
        })
    }
}
impl Move {
    fn parse_moves(s: &str) -> Result<Vec<Move>> {
        s.trim()
            .split(",")
            .map(str::trim)
            .map(|op| op.parse::<Move>())
            .collect()
    }
}


struct CharMap {
    inner: Vec<usize>,
}
impl CharMap {
    fn with_size(size: usize) -> Self {
        assert!(size < 26, "CharMap is limited to ascii");
        let mut inner = Vec::with_capacity(size);
        for i in 0..size {
            inner.push(i);
        }
        Self { inner }
    }

    fn get(&mut self, c: &char) -> usize {
        let ind = ((*c as u8) - b'a') as usize;
        self.inner[ind]
    }

    fn set(&mut self, c: char, v: usize) {
        let ind = ((c as u8) - b'a') as usize;
        self.inner[ind] = v;
    }
}

struct DanceSquad {
    spin_offset: usize,
    progs: Vec<char>,
    // index: HashMap<char, usize>,
    index: CharMap,
}
impl std::str::FromStr for DanceSquad {
    type Err = AnyError;
    fn from_str(s: &str) -> Result<Self> {
        let progs = s.chars().collect::<Vec<_>>();
        let size = progs.len();
        let index = progs.iter()
            .enumerate()
            .fold(CharMap::with_size(size), |mut map, (i, c)| {
                map.set(*c, i);
                map
            });
        let spin_offset = 0;
        Ok(Self { spin_offset, progs, index })
    }
}
impl DanceSquad {
    fn crew(&self) -> String {
        // let size = self.progs.len() as isize;
        // let mut skip = size - (self.spin_offset as isize);
        // while skip < 0 { skip += size }
        // self.progs.iter().cycle().skip(skip as usize).take(size as usize).collect()
        self.progs.iter().collect()
    }

    // fn spin_offset_ind_down(&self, ind: usize) -> usize {
    //     let mut ind = (ind as isize) - (self.spin_offset as isize);
    //     let size = self.progs.len() as isize;
    //     while ind < 0 { ind += size }
    //     ind as usize
    // }

    fn step(&mut self, dance_move: &Move) {
        use Move::*;
        match *dance_move {
            Spin { n } => {
                // self.spin_offset += n;
                for _ in 0..n {
                    let last = self.progs.pop().expect("pop failed");
                    self.progs.insert(0, last);
                }
                for (i, c) in self.progs.iter().enumerate() {
                    self.index.set(*c, i);
                }
            }
            Exchange { ind_a, ind_b } => {
                // let offset_ind_a = self.spin_offset_ind_down(ind_a);
                // let offset_ind_b = self.spin_offset_ind_down(ind_b);
                // println!("{}, {} - {} - {}, {}", ind_a, ind_b, self.spin_offset, offset_ind_a, offset_ind_b);
                // let a = self.progs[offset_ind_a];
                // let b = self.progs[offset_ind_b];
                // self.progs[offset_ind_a] = b;
                // self.progs[offset_ind_b] = a;
                // self.index.insert(a, ind_b);
                // self.index.insert(b, ind_a);
                let a = self.progs[ind_a];
                let b = self.progs[ind_b];
                self.progs[ind_a] = b;
                self.progs[ind_b] = a;
                self.index.set(a, ind_b);
                self.index.set(b, ind_a);
            }
            Partner { a, b } => {
                // println!("char-a, char-b: {}, {}", a, b);
                let ind_a = self.index.get(&a);
                let ind_b = self.index.get(&b);
                self.index.set(a, ind_b);
                self.index.set(b, ind_a);
                self.progs[ind_a] = b;
                self.progs[ind_b] = a;
            }
        }
    }
}


fn part1(programs: &str, moves: &str) -> Result<String> {
    let mut squad = programs.parse::<DanceSquad>()?;
    let moves = Move::parse_moves(moves)?;
    for dance_move in &moves {
        squad.step(dance_move);
        // let crew = squad.crew();
        // print!("{:?} - {:?}", dance_move, squad.crew());
        // let mut chars = crew.chars().collect::<Vec<_>>();
        // chars.sort();
        // chars.dedup();
        // print!(" dup: {}", chars.len() != 16);

        // use std::io::Write;
        // std::io::stdout().flush().unwrap();
        // let mut s = String::new();
        // std::io::stdin().read_line(&mut s).unwrap();
    }
    Ok(squad.crew())
}


fn part2(programs: &str, moves: &str) -> Result<String> {
    let mut squad = programs.parse::<DanceSquad>()?;
    let moves = Move::parse_moves(moves)?;
    let mut count = 0;
    for i in 0..1_000_000_000 {
        for dance_move in &moves {
            squad.step(dance_move);
        }
        if i % 1_000== 0 {
            count += 1;
            println!("{} of 1000", count);
        }
    }
    Ok(squad.crew())
}


pub fn main() {
    let progs = "abcdefghijklmnop";
    println!("d16-p1: {}", part1(progs, INPUT).expect("Part1 error"));
    println!("d16-p2: {}", part2(progs, INPUT).expect("Part2 error"));
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &'static str = "s1,x3/4,pe/b";
    static TEST_PROGS: &'static str = "abcde";

    #[test]
    fn p1() {
        let order = part1(TEST_PROGS, TEST_INPUT).expect("part1 error");
        assert_eq!(order, "baedc");
    }
}

