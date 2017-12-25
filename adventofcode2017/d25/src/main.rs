/*!
http://adventofcode.com/2017/day/25
*/
static INPUT: &'static str = include_str!("../input.txt");
static TEST_INPUT: &'static str = r##"
Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
"##;


mod charmap;
mod tape;

use charmap::AsciiLowerMap;
use tape::BitTape;

type Error = Box<std::error::Error>;
type Result<T> = std::result::Result<T, Error>;


pub enum Dir {
    Left, Right,
}
impl std::str::FromStr for Dir {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(match s.trim().to_lowercase().as_str() {
            "left" => Dir::Left,
            "right" => Dir::Right,
            _ => Err(format!("Invalid direction: {}", s))?,
        })
    }
}


struct Action {
    write: bool,
    move_dir: Dir,
    next_state: char,
}
impl std::str::FromStr for Action {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let mut lines = s.trim().lines();
        let write = lines.next()
            .ok_or_else(|| "Missing write line")?
            .trim()
            .trim_left_matches("- Write the value ")
            .trim_right_matches(".")
            .parse::<u8>()? == 1;
        let move_dir = lines.next()
            .ok_or_else(|| "Missing move line")?
            .trim()
            .trim_left_matches("- Move one slot to the ")
            .trim_right_matches(".")
            .parse::<Dir>()?;
        let next_state = lines.next()
            .ok_or_else(|| "Missing continue state line")?
            .trim()
            .trim_left_matches("- Continue with state ")
            .trim_right_matches(".")
            .to_lowercase()
            .chars().next().ok_or_else(|| "Missing next state")?;
        Ok(Self {
            write,
            move_dir,
            next_state,
        })
    }
}


struct State {
    name: char,
    when_true: Action,
    when_false: Action,
}
impl std::str::FromStr for State {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let mut lines = s.trim().lines();
        let name = lines.next()
            .ok_or_else(|| "Missing name line")?
            .trim()
            .trim_left_matches("In state ")
            .trim_right_matches(":")
            .to_lowercase()
            .chars().next().ok_or_else(|| "Missing state name")?;

        let _ = lines.next().unwrap();
        let when_false = (0..3).fold(String::new(), |mut acc, _| {
            acc.push_str(lines.next().expect("missing action line"));
            acc.push('\n');
            acc
        });
        let when_false = when_false.parse::<Action>()?;

        let _ = lines.next().unwrap();
        let when_true = (0..3).fold(String::new(), |mut acc, _| {
            acc.push_str(lines.next().expect("missing action line"));
            acc.push('\n');
            acc
        });
        let when_true = when_true.parse::<Action>()?;

        Ok(Self {
            name,
            when_true,
            when_false,
        })
    }
}


struct Program {
    start_state: char,
    steps_required: usize,
    states: AsciiLowerMap<State>,
    tape: BitTape,
    step_count: usize,
}
impl std::str::FromStr for Program {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let mut chunks = s.trim().split("\n\n");
        let mut header = chunks.next().ok_or_else(|| "Missing header")?.lines();
        let start_state = header.next()
            .ok_or_else(|| "Missing start state line")?
            .trim()
            .trim_left_matches("Begin in state ")
            .trim_right_matches(".")
            .to_lowercase()
            .chars().next().ok_or_else(|| "Missing start state")?;
        let steps_required = header.next()
            .ok_or_else(|| "Missing diagnostic info line")?
            .trim()
            .trim_left_matches("Perform a diagnostic checksum after ")
            .trim_right_matches(" steps.")
            .parse::<usize>()?;
        let mut states = AsciiLowerMap::new();
        for chunk in chunks {
            let state = chunk.parse::<State>()?;
            states.set(state.name, state);
        }
        Ok(Self {
            start_state,
            steps_required,
            states,
            tape: BitTape::with_size(1_000),
            step_count: 0,
        })
    }
}
impl Program {
    fn checksum(&self) -> usize {
        self.tape.count_ones()
    }

    fn run_to_required_steps(&mut self) {
        let mut state_name = self.start_state;
        while self.step_count < self.steps_required {
            let state = self.states.get(state_name).expect("missing state");
            let current = self.tape.get();
            let action = if current {
                &state.when_true
            } else {
                &state.when_false
            };
            self.tape.set(action.write);
            self.tape.step(&action.move_dir);
            state_name = action.next_state;
            self.step_count += 1;
        }
    }
}


fn part1(input: &str) -> Result<usize> {
    let mut prog = input.parse::<Program>()?;
    prog.run_to_required_steps();
    Ok(prog.checksum())
}


fn time<T, F: Fn() -> Result<T>>(f: F) -> Result<(f64, T)> {
    use std::time;
    let start = time::Instant::now();
    let res = f()?;
    let elap = start.elapsed();
    let ms = elap.as_secs() as f64 * 1_000. + elap.subsec_nanos() as f64 / 1_000_000.;
    Ok((ms, res))
}


fn run() -> Result<()> {
    let (ms, t1) = time(|| Ok(part1(TEST_INPUT)?))?;
    println!("d24-t1: [{:>12}ms] {}", ms, t1);

    let (ms, p1) = time(|| Ok(part1(INPUT)?))?;
    println!("d24-p1: [{:>12}ms] {}", ms, p1);

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

    #[test]
    fn p1() {
        let mut prog = TEST_INPUT.parse::<Program>().unwrap();
        prog.run_to_required_steps();
        assert_eq!(3, prog.checksum());
    }
}

