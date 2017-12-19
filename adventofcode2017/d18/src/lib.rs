#[macro_use] extern crate log;


mod part1;
mod part2;

use std::thread;
use std::sync::mpsc;

pub type Error = Box<std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;


/// Map for lower ascii keys (a-z)
struct AsciiLowerCharMap {
    inner: Vec<i64>,
}
impl AsciiLowerCharMap {
    fn new() -> Self {
        Self { inner: vec![0; 26] }
    }

    #[inline(always)]
    fn as_index(c: char) -> usize {
        ((c as u8) - b'a') as usize
    }

    fn set(&mut self, c: char, val: i64) {
        self.inner[Self::as_index(c)] = val;
    }

    fn get(&self, c: char) -> i64 {
        self.inner[Self::as_index(c)]
    }
}


/// A value that could either be a literal value or stored in a register
#[derive(Debug, Clone)]
pub enum Operand {
    Register(char),
    Value(i64),
}
impl std::str::FromStr for Operand {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        use Operand::*;
        Ok(match s.parse::<i64>() {
            Ok(n) => Value(n),
            Err(_) => {
                let register = s.chars().next().ok_or_else(|| "empty operand string")?;
                Register(register)
            }
        })
    }
}


pub fn part1(input: &str) -> Result<i64> {
    let instrs = part1::parse_instructions(input)?;
    let mut cpu = part1::Cpu::with_instructions(instrs);
    Ok(cpu.run_to_recover()?)
}


pub fn part2(input: &str) -> Result<u64> {
    let instrs = part2::parse_instructions(input)?;
    let (tx_0, rx_0) = mpsc::channel();
    let (tx_1, rx_1) = mpsc::channel();
    let (done_send, done_recv) = mpsc::channel();

    let mut cpu_0 = part2::Cpu::new(instrs.clone(), 0, tx_0.clone(), rx_1, None);
    let mut cpu_1 = part2::Cpu::new(instrs.clone(), 1, tx_1.clone(), rx_0, Some(done_send));
    let h0 = thread::spawn(move || cpu_0.run_to_stop().ok() );
    let h1 = thread::spawn(move || cpu_1.run_to_stop().ok() );

    // wait until cpu1 deadlocks or completes and sends back it's sendcount
    let send_count = done_recv.recv()?;

    // kill cpu0
    tx_1.send(part2::ValOrDie::Die)?;
    h0.join().map_err(|_| "Failed to join h0/cpu0")?;

    // kill cpu1
    tx_0.send(part2::ValOrDie::Die)?;
    h1.join().map_err(|_| "Failed to join h1/cpu1")?;

    Ok(send_count)
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT_1: &'static str = r##"
set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2
"##;

    #[test]
    fn p1() {
        assert_eq!(4, part1(TEST_INPUT_1).expect("p1 fail"));
    }

    static TEST_INPUT_2: &'static str = r##"
snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d
"##;
    #[test]
    fn p2() {
        assert_eq!(3, part2(TEST_INPUT_2).expect("p2 fail"));
    }
}
