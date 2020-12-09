use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;
use std::str::FromStr;

#[derive(Debug, Clone)]
enum Op {
    Nop(isize),
    Acc(i64),
    Jmp(isize),
}
impl FromStr for Op {
    type Err = err::Error;
    fn from_str(op: &str) -> err::Result<Self> {
        Ok(match op.split_whitespace().collect_tuple() {
            Some(("nop", n)) => Op::Nop(n.parse::<isize>()?),
            Some(("acc", n)) => Op::Acc(n.parse::<i64>()?),
            Some(("jmp", n)) => Op::Jmp(n.parse::<isize>()?),
            _ => return Err(format!("unexpected op: {:?}", op).into()),
        })
    }
}
impl Op {
    fn swap(&self) -> Self {
        match *self {
            Op::Nop(n) => Op::Jmp(n),
            Op::Jmp(n) => Op::Nop(n),
            Op::Acc(n) => Op::Acc(n),
        }
    }

    fn is_swappable(&self) -> bool {
        match *self {
            Op::Nop(_) => false,
            Op::Jmp(_) => true,
            Op::Acc(_) => true,
        }
    }
}

#[derive(Clone)]
struct Instr {
    pub op: Op,
    seen: bool,
}

enum Complete {
    Ok(i64),
    Loop(i64),
}

#[derive(Clone)]
struct Vm {
    pub instructions: Vec<Instr>,
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
        for instr in self.instructions.iter_mut() {
            instr.seen = false;
        }
    }

    fn run_to_completion(&mut self) -> err::Result<Complete> {
        loop {
            if self.ptr < 0 {
                return Err(format!("under-flowed instructions: {}", self.ptr).into());
            } else if self.ptr > (self.instructions.len() - 1) as isize {
                return Ok(Complete::Ok(self.accumulator));
            }
            let instr = self.instructions.get_mut(self.ptr as usize).unwrap();
            if instr.seen {
                return Ok(Complete::Loop(self.accumulator));
            }
            instr.seen = true;
            match instr.op {
                Op::Nop(_) => self.ptr += 1,
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

fn modify_next_nop_jmp(vm: &mut Vm, prev_nop_jmp_index: isize) -> err::Result<isize> {
    if prev_nop_jmp_index >= 0 {
        let prev = vm
            .instructions
            .get_mut(prev_nop_jmp_index as usize)
            .unwrap();
        prev.op = prev.op.swap();
    }
    let split_index = prev_nop_jmp_index + 1;
    let (_seen, to_search) = vm.instructions.split_at_mut(split_index as usize);
    for (search_index, instr) in to_search.iter_mut().enumerate() {
        match instr.op {
            Op::Nop(_) | Op::Jmp(_) => {
                instr.op = instr.op.swap();
                let modified_index = split_index + search_index as isize;
                return Ok(modified_index);
            }
            _ => continue,
        }
    }
    Err("no more nop's/jmp's found".into())
}

fn parse(input: &str) -> err::Result<Vm> {
    Ok(Vm::from_code(input)?)
}

fn part1(vm: &mut Vm) -> err::Result<i64> {
    match vm.run_to_completion()? {
        Complete::Loop(n) => Ok(n),
        _ => Err("expected completion by infinite loop".into()),
    }
}

fn part2(vm: &mut Vm) -> err::Result<i64> {
    let mut modified_index = -1;
    loop {
        vm.reset();
        modified_index = modify_next_nop_jmp(vm, modified_index)?;
        match vm.run_to_completion()? {
            Complete::Ok(n) => return Ok(n),
            Complete::Loop(_) => continue,
        }
    }
}

#[allow(unused)]
fn part2_parallel(vm: &mut Vm) -> err::Result<i64> {
    vm.reset();
    let tp = threadpool::ThreadPool::new(num_cpus::get());
    let (res_in, res_out) = std::sync::mpsc::channel();
    let done = std::sync::Arc::new(std::sync::Mutex::new(false));
    for (i, instr) in vm.instructions.iter().enumerate() {
        if instr.op.is_swappable() {
            let mut fork = vm.clone();
            let instr = fork.instructions.get_mut(i).unwrap();
            instr.op = instr.op.swap();
            let ch = res_in.clone();
            let done = done.clone();
            tp.execute(move || {
                {
                    if *done.lock().unwrap() {
                        return;
                    }
                }
                if let Complete::Ok(n) = fork.run_to_completion().expect("error executing fork") {
                    let mut done_flag = done.lock().unwrap();
                    if !*done_flag {
                        ch.send(n).unwrap();
                        *done_flag = true
                    }
                }
            })
        }
    }
    let res = res_out.recv().unwrap();
    Ok(res)
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
    let (ms, res) = time!(part2(&mut input)?);
    println!("  -> p2[{}ms]: {}", ms, res);
    // let (ms, res) = time!(part2_parallel(&mut input)?);
    // println!("  -> p2 parallel[{}ms]: {}", ms, res);

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

    #[test]
    fn test_p2() {
        let mut input = parse(INPUT).expect("parse fail");
        assert_eq!(part2(&mut input).expect("p2 fail"), 8);
    }
}
