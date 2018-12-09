use crate::utils::{StdResult, StdError};
use std::collections::HashMap;
use regex::Regex;


struct Game {
    players: usize,
    marbles: usize,
}
impl std::str::FromStr for Game {
    type Err = StdError;
    fn from_str(input: &str) -> StdResult<Self> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"(?P<players>\d+) players;.*worth (?P<marbles>\d+) points").unwrap();
        }
        let caps = RE.captures(input).ok_or_else(|| "no captures found")?;
        let players = caps.name("players")
            .ok_or_else(|| "no players found")?
            .as_str()
            .parse::<usize>()?;
        let marbles = caps.name("marbles")
            .ok_or_else(|| "no marbles found")?
            .as_str()
            .parse::<usize>()?;
        Ok(Self {
            players,
            marbles,
        })
    }
}


struct Circle {
    _size: usize,
    inner: Vec<usize>,
    current_ind: usize,
}
impl Circle {
    fn new(size: usize) -> Self {
        let mut inner = Vec::with_capacity(size);
        inner.push(0);
        Self { _size: size, inner, current_ind: 0 }
    }

    #[inline]
    fn size(&self) -> usize { self.inner.len() }

    fn clockwise(&self, n: usize) -> usize {
        (self.current_ind + n) % self.size()
    }

    fn counter_clockwise(&self, n: usize) -> usize {
        let rev_ind = self.size() - 1 - self.current_ind;
        let next_ind = rev_ind + n;
        let rem = next_ind % self.size();
        let fwd_ind = self.size() - 1 - rem;
        fwd_ind
    }

    fn insert(&mut self, n: usize) -> u32 {
        if n % 23 == 0 {
            let pop_ind = self.counter_clockwise(7);
            let popped = self.inner.remove(pop_ind);
            self.current_ind = pop_ind;
            return (popped + n) as u32
        }

        let insert_ind = self.clockwise(2);
        if insert_ind > self.size() {
            self.inner.push(n);
        } else {
            self.inner.insert(insert_ind, n);
        }
        self.current_ind = insert_ind;
        0
    }
}

fn part_1(game: &Game) -> StdResult<u32> {
    let mut circle = Circle::new(game.marbles);
    let mut players = map!();
    for (n, p) in (0..game.players).cycle().enumerate() {
        if n > game.marbles {
            break
        }
        let n = n + 1;
        let score = circle.insert(n);
        let e = players.entry(p).or_insert(0);
        *e += score;
    }
    Ok(*players.values().max().ok_or_else(|| "no max!")?)
}

pub fn run() -> StdResult<()> {
    info!("*** Day 9 ***");

    let input = input_file!("d09.txt");
    let game = input.parse::<Game>()?;
    let (ms1, p1) = time!({ part_1(&game)? });
    info!("p1: {}", p1);

//    game.marbles *= 100;
//    let (ms2, p2) = time!({ part_1(&game)? });
//    info!("p2: {}", p2);

    info!("[Day 9 runtimes] p1: {}ms\n", ms1);
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: Vec<(&'static str, u32)> = vec![
            ("10 players; last marble is worth 25 points", 32),
            ("10 players; last marble is worth 1618 points", 8317),
            ("13 players; last marble is worth 7999 points", 146373),
            ("17 players; last marble is worth 1104 points", 2764),
            ("21 players; last marble is worth 6111 points", 54718),
            ("30 players; last marble is worth 5807 points", 37305),
        ];
    }

    #[test]
    fn test_part_1() {
        for (s, expected) in INPUT.iter() {
            let game = s.parse::<Game>().unwrap();
            let res = part_1(&game).unwrap();
            assert_eq!(res, *expected, "failed input: {}", s);
        }
    }
}
