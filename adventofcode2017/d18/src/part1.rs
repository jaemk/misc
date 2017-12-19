use std;
use {Result, Error, AsciiLowerCharMap, Operand};


pub enum Instruction {
    Set(char, Operand),
    Add(char, Operand),
    Mul(char, Operand),
    Mod(char, Operand),
    Sound(char),
    Recover(char),
    Jump(Operand, Operand),
}
impl std::str::FromStr for Instruction {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let mut parts = s.trim().split_whitespace();
        let op = parts.next().ok_or_else(|| "empty op")?;

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
                let operand = parts.next().ok_or_else(|| "missing operand")?;
                Set(register, operand.parse::<Operand>()?)
            }
            "add" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?;
                Add(register, operand.parse::<Operand>()?)
            }
            "mul" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?;
                Mul(register, operand.parse::<Operand>()?)
            }
            "mod" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?;
                Mod(register, operand.parse::<Operand>()?)
            }
            "snd" => {
                let register = get_register_char(&mut parts)?;
                Sound(register)
            }
            "rcv" => {
                let register = get_register_char(&mut parts)?;
                Recover(register)
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


pub struct Cpu {
    registers: AsciiLowerCharMap,
    instructions: Vec<Instruction>,
    instr_index: isize,
    last_sound: Option<i64>,
}
impl Cpu {
    pub fn with_instructions(instrs: Vec<Instruction>) -> Self {
        Self {
            registers: AsciiLowerCharMap::new(),
            instructions: instrs,
            instr_index: 0,
            last_sound: None,
        }
    }

    fn operand_value(&self, oper: &Operand) -> i64 {
        use Operand::*;
        match *oper {
            Value(val) => val,
            Register(other_reg) => self.registers.get(other_reg),
        }
    }

    pub fn run_to_recover(&mut self) -> Result<i64> {
        let upper = self.instructions.len() as isize;
        loop {
            if self.instr_index < 0 || self.instr_index >= upper {
                Err("Jumped out of instruction set")?
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
                Sound(ref reg) => {
                    let val = self.registers.get(*reg);
                    debug!("Beep, {}!", val);
                    self.last_sound = Some(val);
                }
                Recover(ref reg) => {
                    let val = self.registers.get(*reg);
                    if val != 0 {
                        match self.last_sound {
                            Some(val) => return Ok(val),
                            None => Err("No sound available to recover")?,
                        }
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

