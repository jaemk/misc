use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;
use std::str::FromStr;

enum Op {
    Nop,
    Acc(i64),
    Jmp(isize),
}
impl FromStr for Op {
    type Err = err::Error;
    fn from_str(op: &str) -> err::Result<Self> {
        Ok(match op.split_whitespace().collect_tuple() {
            Some(("nop", _)) => Op::Nop,
            Some(("acc", n)) => Op::Acc(n.parse::<i64>()?),
            Some(("jmp", n)) => Op::Jmp(n.parse::<isize>()?),
            _ => return Err(format!("unexpected op: {:?}", op).into()),
        })
    }
}

struct Instr {
    op: Op,
    seen: bool,
}

struct Vm {
    instructions: Vec<Instr>,
    ptr: isize,
    accumulator: i64,
}
impl Vm {
    fn from_code(code: &str) -> err::Result<Self> {
        let instructions = code
            .trim()
            .lines()
            .map(|line| {
                let op = line.parse::<Op>()?;
                Ok(Instr { op, seen: false })
            })
            .collect::<err::Result<Vec<_>>>()?;
        Ok(Vm {
            instructions,
            ptr: 0,
            accumulator: 0,
        })
    }

    fn reset(&mut self) {
        self.ptr = 0;
        self.accumulator = 0;
    }

    fn run_to_duplicate_instruction(&mut self) -> err::Result<i64> {
        loop {
            let instr = self.instructions.get_mut(self.ptr as usize).unwrap();
            if instr.seen {
                return Ok(self.accumulator);
            }
            instr.seen = true;
            match instr.op {
                Op::Nop => self.ptr += 1,
                Op::Acc(n) => {
                    self.accumulator += n;
                    self.ptr += 1;
                }
                Op::Jmp(n) => {
                    self.ptr += n;
                }
            }
        }
    }
}

fn parse(input: &str) -> err::Result<Vm> {
    Ok(Vm::from_code(input)?)
}

fn part1(vm: &mut Vm) -> err::Result<i64> {
    Ok(vm.run_to_duplicate_instruction()?)
}

fn part2(_vm: &mut Vm) -> err::Result<i64> {
    Ok(1)
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d08.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let mut input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&mut input)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    input.reset();
    let (ms, res) = time!(part2(&mut input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &str = r##"
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
"##;

    #[test]
    fn test_p1() {
        let mut input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&mut input).expect("p1 fail"), 5);
    }

    // #[test]
    // fn test_p2() {
    //     let mut input = parse(INPUT).expect("parse fail");
    //     assert_eq!(part2(&mut input).expect("p2 fail"), 32);
    // }
}
