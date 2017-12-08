use std::collections::HashMap;


static INPUT: &'static str = include_str!("../input.txt");


struct Cpu<'a> {
    max: i64,
    map: std::collections::HashMap<&'a str, i64>,
}
impl<'a> Cpu<'a> {
    fn new() -> Self {
        Self {
            max: 0,
            map: HashMap::new(),
        }
    }

    fn get_or_zero(&mut self, reg: &'a str) -> i64 {
        let e = self.map.entry(reg).or_insert(0);
        *e
    }

    fn apply(&mut self, reg: &'a str, op: &str, amnt: i64) {
        let e = self.map.entry(reg).or_insert(0);
        match op {
            "inc" => *e += amnt,
            "dec" => *e -= amnt,
            _ => panic!("invalid op: {:?}", op),
        }
        if *e > self.max { self.max = *e; }
    }

    fn max_final_register_val(&self) -> i64 {
        self.map.iter().map(|(_, val)| *val).max().expect("No max value found")
    }

    fn max_register_val(&self) -> i64 {
        self.max
    }
}


fn cmp(op: &str, left: i64, right: i64) -> bool {
    match op {
        ">"  => left > right,
        ">=" => left >= right,
        "<"  => left < right,
        "<=" => left <= right,
        "==" => left == right,
        "!=" => left != right,
        _ => panic!("invalid op: {:?}", op),
    }
}


pub fn solve(input: &str) -> (i64, i64) {
    let mut cpu = Cpu::new();
    for line in input.trim().lines() {
        let mut line = line.trim().split_whitespace();
        let reg         = line.next().unwrap();  // b
        let op          = line.next().unwrap();  // inc
        let amnt        = line.next().unwrap().parse::<i64>().expect("Invalid amnt int");  // 5
        let _if         = line.next().unwrap();  // if
        let test_reg    = line.next().unwrap();  // a
        let cmp_op      = line.next().unwrap();  // >
        let cmp_val     = line.next().unwrap().parse::<i64>().expect("Invalid cmp int");  // 1

        let cur_test_val = cpu.get_or_zero(test_reg);
        if cmp(cmp_op, cur_test_val, cmp_val) {
            cpu.apply(reg, op, amnt);
        }
    }
    (cpu.max_register_val(), cpu.max_final_register_val())
}


pub fn main() {
    let (max_overall, max_final) = solve(INPUT);
    println!("d8-p1: {}", max_final);
    println!("d8-p2: {}", max_overall);
}


#[cfg(test)]
mod tests {
    use super::*;
    static TEXT_INPUT: &'static str = r##"
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
"##;

    #[test]
    fn p1() {
        let (_, largest_final) = solve(TEXT_INPUT);
        assert_eq!(largest_final, 1, "Expected largest register value to be 1");
    }

    #[test]
    fn p2() {
        let (largest_overall, _) = solve(TEXT_INPUT);
        assert_eq!(largest_overall, 10, "Expected largest register value to be 10");
    }
}
