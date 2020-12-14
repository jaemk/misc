use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;
use std::arch::x86_64::_popcnt64;
use std::collections::HashMap;

#[derive(Debug)]
struct Op {
    addr: u64,
    value: u64,
}

#[derive(Debug, Default)]
struct MaskedOp {
    one_mask: u64,
    zero_mask: u64,
    open_mask: u64,
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
            mop.one_mask = 0;
            mop.zero_mask = 0;
            mop.open_mask = 0;
            for (i, c) in mask.chars().rev().enumerate() {
                if c == '1' {
                    mop.one_mask |= 1 << i;
                } else if c == '0' {
                    mop.zero_mask |= 1 << i;
                } else if c == 'X' {
                    mop.open_mask |= 1 << i;
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

fn part1(mops: &[MaskedOp], mem: &mut HashMap<u64, u64>) -> err::Result<u64> {
    for mop in mops {
        for op in &mop.ops {
            let value = (op.value | mop.one_mask | mop.zero_mask) ^ mop.zero_mask;
            mem.insert(op.addr, value);
        }
    }

    Ok(mem.values().sum())
}

fn part2(mops: &[MaskedOp], mem: &mut HashMap<u64, u64>) -> err::Result<u64> {
    for mop in mops {
        // preallocate for all permutations
        let mask_count = unsafe { _popcnt64(mop.open_mask as i64) };
        let perm_count = 2usize.checked_pow(mask_count as u32).unwrap();
        let mut mask_inds = Vec::with_capacity(mask_count as usize);
        let mut mask_perms = Vec::with_capacity(perm_count as usize);

        // find all the indices where an open mask is set
        for i in 0..36 {
            if (mop.open_mask & 1 << i) > 0 {
                mask_inds.push(i);
            }
        }
        // println!("{:0b} masks: {:?}", mop.open_mask, mask_inds);

        // set the two variations where the mask is all zero and all 1.
        // `mask_perms` has two complimentary masks to represent the bits
        // that should be overridden to one and the bits that should be
        // overridden to zero.
        // for the first example section:
        //   address: 000000000000000000000000000000101010  (decimal 42)
        //   mask:    000000000000000000000000000000X1001X
        //   result:  000000000000000000000000000000X1101X
        // the four permutations of the X----X mask are represented as
        //   bits-set-to-one  |  bits-set-to-zero
        //    0b0             |   0b100001
        //    0b1             |   0b100000
        //    0b100000        |   0b1
        //    0b100001        |   0
        mask_perms.push((0, mop.open_mask));
        mask_perms.push((mop.open_mask, 0));

        // for each open bit, save the variant where only it is set
        for mask_ind in &mask_inds {
            let mask = 1 << mask_ind;
            let anti_mask = mop.open_mask ^ mask;
            mask_perms.push((mask, anti_mask));
        }
        // println!("{:0b} mask_perms: {:?}", mop.open_mask, mask_perms);

        // when there's more than two open bits in the mask,
        // save all the possible combinations of combination-sizes
        // up to the total number of open bits
        if mask_inds.len() >= 2 {
            for n in 2..mask_inds.len() {
                for combo in mask_inds.iter().combinations(n) {
                    // join the set bits back to a masking int
                    let mask = combo.iter().fold(0, |acc, &&i| acc | 1 << i);
                    let anti_mask = mop.open_mask ^ mask;
                    mask_perms.push((mask, anti_mask));
                }
            }
        }

        // for p in &mask_perms {
        //     println!("{:0b} -- p: {:0b} / {:0b}", mop.open_mask, p.0, p.1);
        // }

        for op in &mop.ops {
            let addr = op.addr | mop.one_mask;
            for (one_mask, zero_mask) in &mask_perms {
                let addr = (addr | one_mask | zero_mask) ^ zero_mask;
                // println!("addr: {:0b} {}", addr, addr);
                mem.insert(addr, op.value);
            }
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

    // cheat since I know how big it ends up
    let mut mem = map!(size = 85000);

    let (ms, res) = time!(part1(&input, &mut mem)?);
    println!("  -> p1[{}ms]: {}", ms, res);

    mem.clear();
    let (ms, res) = time!(part2(&input, &mut mem)?);
    println!("  -> p2[{}ms]: {}", ms, res);

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
        assert_eq!(input[0].one_mask, 0b1000000);
        assert_eq!(input[0].zero_mask, 0b10);
        assert_eq!(input[0].ops[0].addr, 8);
        assert_eq!(input[0].ops[0].value, 11);
    }

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input, &mut map!()).expect("p1 fail"), 165)
    }

    static INPUT_2: &str = r##"
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
"##;

    #[test]
    fn test_p2() {
        let input = parse(INPUT_2).expect("parse fail");
        assert_eq!(part2(&input, &mut map!()).expect("p2 fail"), 208);
    }
}
