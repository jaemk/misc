use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;
use std::collections::HashSet;

#[derive(Debug, Hash, Eq, PartialEq)]
struct Rule<'a> {
    name: &'a str,
    ranges: [(u32, u32); 2],
}

#[derive(Debug)]
struct Input<'a> {
    rules: Vec<Rule<'a>>,
    my_ticket: Vec<u32>,
    other_tickets: Vec<Vec<u32>>,
}

fn parse(input: &str) -> err::Result<Input> {
    let (rule_section, my_ticket_section, other_tickets_section) =
        input.trim().split("\n\n").collect_tuple().unwrap();
    let rules = rule_section
        .trim()
        .lines()
        .map(|line| {
            let (name, ranges) = line.split(':').collect_tuple().unwrap();
            let (range1, range2) = ranges.split("or").collect_tuple().unwrap();

            let (range1a, range1b) = range1.split('-').collect_tuple().unwrap();
            let range1a = range1a.trim().parse::<u32>()?;
            let range1b = range1b.trim().parse::<u32>()?;

            let (range2a, range2b) = range2.split('-').collect_tuple().unwrap();
            let range2a = range2a.trim().parse::<u32>()?;
            let range2b = range2b.trim().parse::<u32>()?;
            Ok(Rule {
                name,
                ranges: [(range1a, range1b), (range2a, range2b)],
            })
        })
        .collect::<err::Result<Vec<_>>>()?;
    let (_, my_ticket) = my_ticket_section.trim().lines().collect_tuple().unwrap();
    let my_ticket = my_ticket
        .trim()
        .split(',')
        .map(|n| Ok(n.parse::<u32>()?))
        .collect::<err::Result<Vec<_>>>()?;

    let (_, other_tickets) = other_tickets_section.split(':').collect_tuple().unwrap();
    let other_tickets = other_tickets
        .trim()
        .lines()
        .map(|line| {
            Ok(line
                .trim()
                .split(',')
                .map(|n| Ok(n.parse::<u32>()?))
                .collect::<err::Result<Vec<_>>>()?)
        })
        .collect::<err::Result<Vec<_>>>()?;
    Ok(Input {
        rules,
        my_ticket,
        other_tickets,
    })
}

fn part1(input: &mut Input) -> err::Result<u32> {
    let mut err_rate = 0;
    let mut tickets = vec![];
    std::mem::swap(&mut tickets, &mut input.other_tickets);
    for ticket in &tickets {
        let mut valid = true;

        for &v in ticket {
            let mut fails = 0;
            for rule in &input.rules {
                if (v < rule.ranges[0].0 || v > rule.ranges[0].1)
                    && (v < rule.ranges[1].0 || v > rule.ranges[1].1)
                {
                    fails += 1;
                }
            }
            if fails == input.rules.len() {
                err_rate += v;
                valid = false;
            }
        }
        if valid {
            input.other_tickets.push(ticket.clone());
        }
    }
    Ok(err_rate)
}

fn passes_rule(rule: &Rule, v: u32) -> bool {
    (v >= rule.ranges[0].0 && v <= rule.ranges[0].1)
        || v >= rule.ranges[1].0 && v <= rule.ranges[1].1
}

fn part2(input: &Input) -> err::Result<u64> {
    // find all rules that pass for each index of the tickets
    // e.g. rule-A is "passing" for index-0 when all ticket[0] values
    //      pass rule-A
    let mut passing_rules = vec![];

    // also equals the number of rules
    let ticket_size = input.my_ticket.len();

    for i in 0..ticket_size {
        let passing_for_index = input
            .rules
            .iter()
            .filter(|&r| {
                input
                    .other_tickets
                    .iter()
                    .all(|ticket| passes_rule(r, ticket[i]))
            })
            .collect::<HashSet<_>>();
        passing_rules.push(passing_for_index);
    }

    // I'm surprised this works, every rule applies to only one ticket column
    let mut picked = set![];
    loop {
        let mut done = true;

        #[allow(clippy::needless_range_loop)]
        // for every ticket value/rule
        for i in 0..ticket_size {
            if passing_rules[i].len() > 1 {
                // this ticket-index still has multiple valid rules
                done = false;
                for &(j, p) in picked.iter() {
                    // we've already "picked" this index
                    if i == j {
                        continue;
                    }
                    passing_rules[i].remove(p);
                }
            }
            if passing_rules[i].len() == 1 {
                picked.insert((i, passing_rules[i].iter().cloned().next().unwrap()));
            }
        }
        if done {
            break;
        }
    }

    let mut val = 1;
    for &(i, r) in picked.iter() {
        // println!("{}: {}", r.name, input.my_ticket[i]);
        if r.name.starts_with("departure") {
            val *= input.my_ticket[i] as u64;
        }
    }
    Ok(val)
}

pub fn run() -> err::Result<()> {
    let raw_input = time!(
        file::read("../input/d16.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );

    let mut input = time!(
        parse(&raw_input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&mut input)?);
    println!("  -> p1[{}ms]: {}", ms, res);

    let (ms, res) = time!(part2(&input)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &str = r##"
class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
"##;

    #[test]
    fn test_p1() {
        let mut input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&mut input).expect("p1 fail"), 71);
    }

    static INPUT2: &str = r##"
departure class: 0-1 or 4-19
row: 0-5 or 8-19
departure seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
"##;

    #[test]
    fn test_p2() {
        let mut input = parse(INPUT2).expect("parse fail");
        assert_eq!(part1(&mut input).expect("p1 fail"), 0);
        assert_eq!(part2(&input).expect("p2 fail"), 12 * 13);
    }
}
