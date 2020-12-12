use crate::utils::err;
use crate::utils::file;
use std::str::FromStr;

#[derive(Copy, Clone)]
enum Move {
    L(i64),
    R(i64),
    F(i64),
    N(i64),
    S(i64),
    E(i64),
    W(i64),
}

impl FromStr for Move {
    type Err = err::Error;
    fn from_str(s: &str) -> err::Result<Move> {
        use Move::*;

        let (m, amount) = s.split_at(1);
        Ok(match m {
            "N" => N(amount.parse()?),
            "S" => S(amount.parse()?),
            "E" => E(amount.parse()?),
            "W" => W(amount.parse()?),
            "F" => F(amount.parse()?),
            "L" => L(amount.parse::<i64>()? / 90),
            "R" => R(amount.parse::<i64>()? / 90),
            _ => return Err(format!("unknown move: {}, {}", m, amount).into()),
        })
    }
}

#[derive(Copy, Clone)]
enum D {
    N,
    S,
    E,
    W,
}
impl D {
    fn left(&self) -> Self {
        use D::*;
        match *self {
            N => W,
            W => S,
            S => E,
            E => N,
        }
    }

    fn right(&self) -> Self {
        use D::*;
        match *self {
            N => E,
            E => S,
            S => W,
            W => N,
        }
    }
}

struct Ship {
    x: i64,
    y: i64,
    facing: D,
}
impl Ship {
    fn new() -> Self {
        Ship {
            x: 0,
            y: 0,
            facing: D::E,
        }
    }

    fn move_dir(&mut self, d: D, amount: i64) {
        match d {
            D::N => self.y -= amount,
            D::E => self.x += amount,
            D::S => self.y += amount,
            D::W => self.x -= amount,
        }
    }

    fn step(&mut self, m: &Move) {
        match m {
            Move::N(n) => self.move_dir(D::N, *n),
            Move::E(n) => self.move_dir(D::E, *n),
            Move::S(n) => self.move_dir(D::S, *n),
            Move::W(n) => self.move_dir(D::W, *n),
            Move::F(n) => self.move_dir(self.facing, *n),
            Move::L(times) => {
                for _ in 0..*times {
                    self.facing = self.facing.left()
                }
            }
            Move::R(times) => {
                for _ in 0..*times {
                    self.facing = self.facing.right()
                }
            }
        }
    }

    fn distance(&self) -> i64 {
        self.x.abs() + self.y.abs()
    }
}

fn parse(input: &str) -> err::Result<Vec<Move>> {
    Ok(input
        .trim()
        .lines()
        .map(|line| Ok(line.parse::<Move>()?))
        .collect::<err::Result<Vec<_>>>()?)
}

fn part1(moves: &[Move]) -> err::Result<i64> {
    let mut ship = Ship::new();
    for m in moves {
        ship.step(m);
    }
    Ok(ship.distance())
}

fn part2(moves: &[Move]) -> err::Result<i64> {
    Ok(1)
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d12.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &str = r##"
F10
N3
F7
R90
F11
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 25);
    }

    // #[test]
    // fn test_p2() {
    //     let input = parse(INPUT).expect("parse fail");
    //     assert_eq!(part2(&input).expect("p2 fail"), 26);
    // }
}
