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
    progs: Vec<char>,
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
        Ok(Self { progs, index })
    }
}
impl DanceSquad {
    fn crew(&self) -> String {
        self.progs.iter().collect()
    }

    fn step(&mut self, dance_move: &Move) {
        use Move::*;
        match *dance_move {
            Spin { n } => {
                for _ in 0..n {
                    let last = self.progs.pop().expect("pop failed");
                    self.progs.insert(0, last);
                }
                for (i, c) in self.progs.iter().enumerate() {
                    self.index.set(*c, i);
                }
            }
            Exchange { ind_a, ind_b } => {
                let a = self.progs[ind_a];
                let b = self.progs[ind_b];
                self.progs[ind_a] = b;
                self.progs[ind_b] = a;
                self.index.set(a, ind_b);
                self.index.set(b, ind_a);
            }
            Partner { a, b } => {
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
    }
    Ok(squad.crew())
}


fn part2_cycle(programs: &str, moves: &str) -> Result<String> {
    let mut squad = programs.parse::<DanceSquad>()?;
    let moves = Move::parse_moves(moves)?;
    let mut cache: HashMap<String, (usize, String)> = HashMap::new();

    let iters = 1_000_000_000;
    for i in 0..iters {
        let start_crew = squad.crew();
        if let Some(&(ref index, ref cached_after)) = cache.get(&start_crew) {
            let cycle_size = i - index;
            let remaining = iters - i;
            if remaining % cycle_size == 0 {
                return Ok(start_crew)
            }
            squad = cached_after.parse::<DanceSquad>()?;
            continue
        }
        for dance_move in &moves {
            squad.step(dance_move);
        }
        cache.insert(start_crew, (i, squad.crew()));
    }
    Ok(squad.crew())
}


fn part2_brute(programs: &str, moves: &str) -> Result<String> {
    let mut squad = programs.parse::<DanceSquad>()?;
    let moves = Move::parse_moves(moves)?;
    let mut cache: HashMap<String, String> = HashMap::new();

    let mut count = 0;
    for i in 0..1_000_000_000 {
        if i % 1_000_000 == 0 {
            println!("{} of 1000 rounds", count);
            count += 1;
        }
        let start_crew = squad.crew();
        if let Some(cached_after) = cache.get(&start_crew) {
            squad = cached_after.parse::<DanceSquad>()?;
            continue
        }
        for dance_move in &moves {
            squad.step(dance_move);
        }
        cache.insert(start_crew, squad.crew());
    }
    Ok(squad.crew())
}


pub fn main() {
    let progs = "abcdefghijklmnop";
    println!("d16-p1: {}", part1(progs, INPUT).expect("Part1 error"));
    println!("d16-p2 cycle-detection: {}", part2_cycle(progs, INPUT).expect("Part2 error"));
    println!("\nBrute force Part 2:");
    println!("d16-p2 brute force: {}", part2_brute(progs, INPUT).expect("Part2 error"));
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

