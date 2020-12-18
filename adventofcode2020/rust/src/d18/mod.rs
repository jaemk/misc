use crate::utils::err;
use crate::utils::file;
use itertools::{Itertools, MinMaxResult};
use std::collections::HashSet;

#[derive(Debug, Copy, Clone)]
enum Op {
    Add,
    Mul,
}

#[derive(Debug, Clone)]
struct Group(Vec<Value>);

#[derive(Debug, Clone)]
enum Value {
    Group(Group),
    Number(u64),
    Operation(Op),
}

fn parse(input: &str) -> err::Result<Vec<Group>> {
    fn parse_value(input: &[char]) -> err::Result<Value> {
        let mut values = vec![];
        let mut i = 0;
        loop {
            if i >= input.len() {
                break;
            }
            if input[i] == '(' {
                i += 1;
                let group_start = i;
                let mut openings = 1;
                let mut done_group = false;
                while i < input.len() && !done_group {
                    if input[i] == ')' && openings == 1 {
                        // capture subgroup
                        let value = parse_value(&input[group_start..i])?;
                        values.push(value);
                        done_group = true;
                    } else if input[i] == ')' {
                        openings -= 1;
                    } else if input[i] == '(' {
                        openings += 1;
                    }
                    i += 1;
                }
            } else if input[i] == '+' {
                values.push(Value::Operation(Op::Add));
                i += 1;
            } else if input[i] == '*' {
                values.push(Value::Operation(Op::Mul));
                i += 1;
            } else if input[i] == ' ' {
                i += 1;
            } else {
                let start = i;
                while i < input.len() && input[i] != ' ' && input[i] != '(' && input[i] != ')' {
                    i += 1;
                }
                let n = input[start..i].iter().collect::<String>().parse()?;
                values.push(Value::Number(n));
            }
        }
        Ok(Value::Group(Group(values)))
    }

    let char_groups = input
        .trim()
        .lines()
        .map(|line| line.chars().collect_vec())
        .collect_vec();
    let mut groups = vec![];
    for char_group in &char_groups {
        let value = parse_value(&char_group)?;
        if let Value::Group(g) = value {
            groups.push(g)
        } else {
            return Err("failed parsing".into());
        }
    }
    Ok(groups)
}

fn eval(n1: u64, n2: u64, op: Op) -> u64 {
    match op {
        Op::Add => n1 + n2,
        Op::Mul => n1 * n2,
    }
}

fn eval_group(group: &Group) -> u64 {
    let vals = &group.0;
    let mut prev = None;
    let mut op = None;
    for v in vals {
        match v {
            Value::Operation(o) => op = Some(*o),
            Value::Number(n) => {
                if let Some(p) = prev {
                    prev = Some(eval(p, *n, op.unwrap()));
                } else {
                    prev = Some(*n);
                }
            }
            Value::Group(ref g) => {
                let v = eval_group(g);
                if let Some(p) = prev {
                    prev = Some(eval(p, v, op.unwrap()));
                } else {
                    prev = Some(v);
                }
            }
        }
    }
    prev.unwrap()
}

fn part1(groups: &[Group]) -> err::Result<u64> {
    let mut res = 0;
    for g in groups {
        let val = eval_group(g);
        println!("{:?}", val);
        res += val;
    }
    Ok(res)
}
fn part2(input: &[Group]) -> err::Result<u64> {
    Ok(1)
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d18.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &[(&str, u64)] = &[("1 + 2 * 3 + 4 * 5 + 6", 71)];

    #[test]
    fn test_p1() {
        for &(input, expected) in INPUT {
            let input = parse(input).expect("parse fail");
            assert_eq!(part1(&input).expect("p1 fail"), expected);
        }
    }

    // #[test]
    // fn test_p2() {
    //     let input = parse(INPUT).expect("parse fail");
    //     let weakness = part1(&input, 5).expect("weakness failed");
    //     assert_eq!(part2(&input, weakness).expect("p2 fail"), 62);
    // }
}
