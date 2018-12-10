use crate::utils::{StdResult, StdError};
use std::collections::{HashMap, LinkedList};
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


enum Move {
    Left(usize),
    Right(usize),
}

struct Circle {
    left: LinkedList<usize>,
    n_left: usize,
    right: LinkedList<usize>,
    n_right: usize,
}
impl Circle {
    fn new() -> Self {
        let mut left = LinkedList::new();
        left.push_front(0);
        Self {
            left,
            n_left: 1,
            right: LinkedList::new(),
            n_right: 0,
        }
    }

    #[inline]
    fn size(&self) -> usize { self.n_left + self.n_right }

    fn rotate(&mut self, move_: Move) {
        match move_ {
            Move::Left(n) => {
                for _ in 0..n {
                    let e = self.left.pop_front().expect("left is empty");
                    self.right.push_front(e);
                }
                self.n_left -= n;
                self.n_right += n;
            }
            Move::Right(n) => {
                for _ in 0..n {
                    let e = self.right.pop_front().expect("right is empty");
                    self.left.push_front(e);
                }
                self.n_right -= n;
                self.n_left += n;
            }
        }
    }

    /// For a clockwise move of n-spaces, return reduced direction of movement
    fn clockwise(&self, n: usize) -> Move {
        // <0> <>  // right(0)
        // <0 1> <> // 1 // 2 % 2 = 0 // 1 - 0 // left(1) // <0> <1>
        // <0 2> <1> // 1 // 2 % 3 = 2 // 2 - 1 // right(1) // <0 2 1>
        // <0 2 1 3> <> // 3 // 4 % 4 = 0 // 3 - 0 // left(3) // <0> <2 1 3>
        // <0 4> <2 1 3> // 1 // 2 % 5 = 2 // 2 - 1 // right(1) // <0 4 2> <1 3>
        // <0 4 2 5> <1 3> // 3 // 4 % 6 // 4 - 3 // right(1) // <0 4 2 5 1> <3>
        // <0 4 2 5 1 6> <3> // 5 /// 6 % 7 // 6 - 5 // right(1) // <0 4 2 5 1 6 3> <>
        // <0 4 2 5 1 6 3 7> <> //
        if self.size() == 1 { return Move::Right(0) }

        let array_pos = self.n_left - 1;
        let new_pos = (array_pos + n) % self.size();
        if new_pos < array_pos { Move::Left(array_pos - new_pos) }
        else { Move::Right(new_pos - array_pos) }
    }

    /// For a counter-clockwise move of n-spaces, return reduced direction of movement
    fn counter_clockwise(&self, n: usize) -> Move {
        // <0 2 3 4> <7 6 5 8 9 11>
        // cc 7 == move::right(3)
        // <0 2 3 4 7 6 5> <8 9 11>
        // 3 -> 10 - 1 - 3 = 6 -> 6 + 7 = 13 -> 13 % 10 = 3 -> 10 - 1 - 3 = 6 -> 6 - 3 = right(3)
        let array_pos = self.n_left - 1;
        let rev_ind = self.size() - 1 - array_pos;
        let next_ind = rev_ind + n;
        let rem = next_ind % self.size();
        let new_pos = self.size() - 1 - rem;
        if new_pos < array_pos { Move::Left(array_pos - new_pos) }
        else { Move::Right(new_pos - array_pos) }
    }

    fn insert(&mut self, n: usize) -> u32 {
        if n % 23 == 0 {
            let move_= self.counter_clockwise(7);
            self.rotate(move_);
            let e = self.left.pop_front().expect("left empty");
            self.n_left -= 1;
            self.rotate(Move::Right(1));
            return (e + n) as u32
        }

        let move_ = self.clockwise(1);
        self.rotate(move_);
        self.left.push_front(n);
        self.n_left += 1;
        0
    }
}


fn solve(game: &Game) -> StdResult<u32> {
    let mut circle = Circle::new();
    let mut players = map!();
    for (n, p) in (0..game.players).cycle().enumerate() {
        if n >= game.marbles {
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
    let mut game = input.parse::<Game>()?;
    let (ms1, p1) = time!({ solve(&game)? });
    info!("p1: {}", p1);

    game.marbles *= 100;
    let (ms2, p2) = time!({ solve(&game)? });
    info!("p2: {}", p2);

    info!("[Day 9 runtimes] p1: {}ms, p2: {}ms\n", ms1, ms2);
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
            let res = solve(&game).unwrap();
            assert_eq!(res, *expected, "failed input: {}", s);
        }
    }
}
