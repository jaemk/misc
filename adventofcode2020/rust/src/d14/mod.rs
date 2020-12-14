use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;

#[derive(Debug)]
struct Op {
    addr: usize,
    value: u64,
}

#[derive(Debug, Default)]
struct MaskedOp {
    and_mask: u64,
    xor_mask: u64,
    ops: Vec<Op>,
}

fn parse(input: &str) -> err::Result<Vec<MaskedOp>> {
    let mut masked_ops = vec![];
    let mut add_to_mops = |mop: MaskedOp| {
        if !mop.ops.is_empty() {
            masked_ops.push(mop);
        }
    };
    let mut mop = MaskedOp::default();
    for line in input.trim().lines() {
        if line.starts_with("mask") {
            let mut new_mop = MaskedOp::default();
            std::mem::swap(&mut mop, &mut new_mop);
            add_to_mops(new_mop);

            let mask = line
                .trim_start_matches("mask")
                .trim()
                .trim_start_matches('=')
                .trim();
            mop.and_mask = 0;
            mop.xor_mask = 0;
            for (i, c) in mask.chars().rev().enumerate() {
                if c == '1' {
                    mop.and_mask |= 1 << i;
                } else if c == '0' {
                    mop.xor_mask |= 1 << i;
                }
            }
        } else {
            let (addr, value) = line.split('=').collect_tuple().unwrap();
            let addr = addr.trim();
            let addr = &addr[4..(addr.len() - 1)];
            let addr = addr.parse()?;
            let value = value.trim().parse::<u64>()?;
            mop.ops.push(Op { addr, value });
        }
    }
    add_to_mops(mop);
    Ok(masked_ops)
}

fn part1(mops: &[MaskedOp]) -> err::Result<u64> {
    let mut mem = map!(size = 500);
    for mop in mops {
        for op in &mop.ops {
            let value = (op.value | mop.and_mask | mop.xor_mask) ^ mop.xor_mask;
            mem.insert(op.addr, value);
        }
    }

    Ok(mem.values().sum())
}

pub fn run() -> err::Result<()> {
    let raw_input = time!(
        file::read("../input/d14.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );

    let input = time!(
        parse(&raw_input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );
    let (ms, res) = time!(part1(&input)?);
    println!("  -> p1[{}ms]: {}", ms, res);

    // let (ms, res) = time!(part2::solve(&input)?);
    // println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &str = r##"
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
"##;

    #[test]
    fn test_parse() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(input[0].and_mask, 0b1000000);
        assert_eq!(input[0].xor_mask, 0b10);
        assert_eq!(input[0].ops[0].addr, 8);
        assert_eq!(input[0].ops[0].value, 11);
    }

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input).expect("p1 fail"), 165)
    }

    // #[test]
    // fn test_p2() {
    //     let input = part2::parse(INPUT).expect("parse fail");
    //     assert_eq!(part2::solve(&input).expect("p2 fail"), 1068781);
    // }
}
