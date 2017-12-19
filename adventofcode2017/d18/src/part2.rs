use std;
use std::sync::mpsc;
use {Result, Error, AsciiLowerCharMap, Operand};


#[derive(Clone)]
pub enum Instruction {
    Set(char, Operand),
    Add(char, Operand),
    Mul(char, Operand),
    Mod(char, Operand),
    Snd(Operand),
    Rcv(char),
    Jump(Operand, Operand),
}
impl std::str::FromStr for Instruction {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let mut parts = s.trim().split_whitespace();
        let op = parts.next().expect("empty op");

        #[inline(always)]
        fn get_register_char(iter: &mut Iterator<Item=&str>) -> Result<char> {
            let c = iter.next().ok_or_else(|| "missing register")?
                .chars().next().ok_or_else(|| "empty register string")?;
            Ok(c)
        }

        use self::Instruction::*;
        let instr = match op {
            "set" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?.parse::<Operand>()?;
                Set(register, operand)
            }
            "add" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?.parse::<Operand>()?;
                Add(register, operand)
            }
            "mul" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?.parse::<Operand>()?;
                Mul(register, operand)
            }
            "mod" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?.parse::<Operand>()?;
                Mod(register, operand)
            }
            "snd" => {
                let operand = parts.next().ok_or_else(|| "missing operand")?.parse::<Operand>()?;
                Snd(operand)
            }
            "rcv" => {
                let register = get_register_char(&mut parts)?;
                Rcv(register)
            }
            "jgz" => {
                let operand_1 = parts.next().ok_or_else(|| "missing operand")?.parse::<Operand>()?;
                let operand_2 = parts.next().ok_or_else(|| "missing operand")?.parse::<Operand>()?;
                Jump(operand_1, operand_2)
            }
            _ => Err(format!("Invalid instruction: {}", s))?,
        };
        Ok(instr)
    }
}


pub fn parse_instructions(s: &str) -> Result<Vec<Instruction>> {
    s.trim()
        .lines()
        .map(|line| Ok(line.trim().parse::<Instruction>()?))
        .collect()
}


pub enum ValOrDie {
    Val(i64),
    Die,
}


pub struct Cpu {
    pid: u32,
    registers: AsciiLowerCharMap,
    instructions: Vec<Instruction>,
    instr_index: isize,
    send_count: u64,
    tx: mpsc::Sender<ValOrDie>,
    rx: mpsc::Receiver<ValOrDie>,
    done_send: Option<mpsc::Sender<u64>>,
}
impl Cpu {
    pub fn new(instructions: Vec<Instruction>,
               pid: u32,
               tx: mpsc::Sender<ValOrDie>,
               rx: mpsc::Receiver<ValOrDie>,
               done_send: Option<mpsc::Sender<u64>>) -> Self {
        let mut registers = AsciiLowerCharMap::new();
        registers.set('p', pid as i64);
        Self {
            pid: pid,
            registers,
            instructions,
            instr_index: 0,
            send_count: 0,
            tx,
            rx,
            done_send,
        }
    }

    /// Return the literal value or the value in the register
    fn operand_value(&self, oper: &Operand) -> i64 {
        use Operand::*;
        match *oper {
            Value(val) => val,
            Register(other_reg) => self.registers.get(other_reg),
        }
    }

    /// Drain the channel until we're told to die
    fn wait_return(&self) -> Result<()> {
        loop {
            match self.rx.recv()? {
                ValOrDie::Die => return Ok(()),
                _ => continue,
            }
        }
    }

    /// Run until a deadlock is detected or the program completes
    pub fn run_to_stop(&mut self) -> Result<()> {
        let upper = self.instructions.len() as isize;
        loop {
            if self.instr_index < 0 || self.instr_index >= upper {
                debug!("pid {}: Complete! ... Sent {} values", self.pid, self.send_count);
                if let Some(ref done_send) = self.done_send {
                    done_send.send(self.send_count)?;
                }
                return self.wait_return();
            }

            use self::Instruction::*;
            match self.instructions[self.instr_index as usize] {
                Set(ref reg, ref oper) => {
                    let val = self.operand_value(oper);
                    self.registers.set(*reg, val);
                }
                Add(ref reg, ref oper) => {
                    let other = self.operand_value(oper);
                    let val = self.registers.get(*reg) + other;
                    self.registers.set(*reg, val)
                }
                Mul(ref reg, ref oper) => {
                    let other = self.operand_value(oper);
                    let val = self.registers.get(*reg) * other;
                    self.registers.set(*reg, val)
                }
                Mod(ref reg, ref oper) => {
                    let other = self.operand_value(oper);
                    let val = self.registers.get(*reg) % other;
                    self.registers.set(*reg, val)
                }
                Snd(ref oper) => {
                    self.send_count += 1;
                    let val = self.operand_value(oper);
                    debug!("{}: Send: {}", self.pid, val);
                    self.tx.send(ValOrDie::Val(val))?;
                }
                Rcv(ref reg) => {
                    let val = self.rx.recv_timeout(std::time::Duration::from_millis(100));
                    let val = match val {
                        Err(_) => {
                            debug!("pid {}: Deadlock! ... Sent {} values", self.pid, self.send_count);
                            if let Some(ref done_send) = self.done_send {
                                done_send.send(self.send_count)?;
                            }
                            return self.wait_return();
                        }
                        Ok(v) => v,
                    };
                    match val {
                        ValOrDie::Val(val) => self.registers.set(*reg, val),
                        ValOrDie::Die => return Ok(()),
                    }
                }
                Jump(ref oper_1, ref oper_2) => {
                    let test = self.operand_value(oper_1);
                    if test > 0 {
                        let jump_val = self.operand_value(oper_2);
                        self.instr_index += jump_val as isize;
                        continue;
                    }
                }
            }
            self.instr_index += 1;
        }
    }
}

