
mod asm;


static INPUT: &'static str = include_str!("../input.txt");


type Error = Box<std::error::Error>;
type Result<T> = std::result::Result<T, Error>;


/// Map for lower ascii keys (a-z)
pub struct AsciiLowerCharMap {
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

    pub fn set(&mut self, c: char, val: i64) {
        self.inner[Self::as_index(c)] = val;
    }

    pub fn get(&self, c: char) -> i64 {
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


pub enum Instruction {
    Set(char, Operand),
    Sub(char, Operand),
    Mul(char, Operand),
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
            "sub" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?;
                Sub(register, operand.parse::<Operand>()?)
            }
            "mul" => {
                let register = get_register_char(&mut parts)?;
                let operand = parts.next().ok_or_else(|| "missing operand")?;
                Mul(register, operand.parse::<Operand>()?)
            }
            "jnz" => {
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
    pub registers: AsciiLowerCharMap,
    instructions: Vec<Instruction>,
    instr_index: isize,
    mul_count: usize,
}
impl Cpu {
    pub fn with_instructions(instrs: Vec<Instruction>) -> Self {
        Self {
            registers: AsciiLowerCharMap::new(),
            instructions: instrs,
            instr_index: 0,
            mul_count: 0,
        }
    }

    fn operand_value(&self, oper: &Operand) -> i64 {
        use Operand::*;
        match *oper {
            Value(val) => val,
            Register(other_reg) => self.registers.get(other_reg),
        }
    }

    /// Run to completion, returning mul-count
    pub fn run_to_end(&mut self) -> Result<usize> {
        let upper = self.instructions.len() as isize;
        loop {
            if self.instr_index < 0 || self.instr_index >= upper {
                // Err("Jumped out of instruction set")?
                return Ok(self.mul_count);
            }

            use self::Instruction::*;
            match self.instructions[self.instr_index as usize] {
                Set(ref reg, ref oper) => {
                    let val = self.operand_value(oper);
                    self.registers.set(*reg, val);
                }
                Sub(ref reg, ref oper) => {
                    let other = self.operand_value(oper);
                    let val = self.registers.get(*reg) - other;
                    self.registers.set(*reg, val)
                }
                Mul(ref reg, ref oper) => {
                    let other = self.operand_value(oper);
                    let val = self.registers.get(*reg) * other;
                    self.mul_count += 1;
                    self.registers.set(*reg, val)
                }
                Jump(ref oper_1, ref oper_2) => {
                    let test = self.operand_value(oper_1);
                    if test != 0 {
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


fn part1(input: &str) -> Result<usize> {
    let instrs = parse_instructions(input)?;
    let mut cpu = Cpu::with_instructions(instrs);
    let mul_count = cpu.run_to_end()?;
    Ok(mul_count)
}


fn part2(_input: &str) -> Result<i64> {
    Ok(asm::run_minimal())
}


fn time<T, F: Fn() -> Result<T>>(f: F) -> Result<(f64, T)> {
    use std::time;
    let start = time::Instant::now();
    let ans = f()?;
    let elap = start.elapsed();
    let ms = elap.as_secs() as f64 * 1_000. + elap.subsec_nanos() as f64 / 1_000_000.;
    Ok((ms, ans))
}


fn run() -> Result<()> {
    let (ms, p1) = time(|| {
        let ans = part1(INPUT)?;
        Ok(ans)
    })?;
    println!("d23-p1: [{:>12}ms] {}", ms, p1);

    let (ms, p2) = time(|| {
        let ans = part2(INPUT)?;
        Ok(ans)
    })?;
    println!("d23-p2: [{:>12}ms] {}", ms, p2);
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
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

