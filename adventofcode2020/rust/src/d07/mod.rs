use crate::utils::err;
use crate::utils::file;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

type Rules = HashMap<String, HashMap<String, usize>>;

fn parse(input: &str) -> err::Result<(Rules, Rules)> {
    Ok(input.trim().lines().fold(
        (map!(), map!()),
        |(mut reverse, mut forward): (Rules, Rules), line| {
            // reverse: colorA -> map<colorB-that-contains-colorA, how-many-colorA's-colorB-holds>
            // forward: colorA -> map<colorB-that-colorA-holds, how-many-colorB's-colorA-holds>

            // pale lime bags contain 3 faded indigo bags, 5 dark indigo bags, 4 dull blue bags, 1 faded lavender bag.
            let (container_color, contents): (&str, &str) =
                line.split(" bags contain ").collect_tuple().unwrap();

            // ("pale lime", "3 faded indigo bags, 5 dark indigo bags, 4 dull blue bags, 1 faded lavender bag.")
            if contents.starts_with("no") {
                return (reverse, forward);
            }

            let fwd_container = forward
                .entry(container_color.to_string())
                .or_insert_with(HashMap::new);
            for content in contents.split(',') {
                // 3 faded indigo bags
                // 1 faded lavender bag.
                let mut bag = content.split_whitespace();
                let count = bag.next().unwrap().parse::<usize>().unwrap();
                let color = bag.take(2).join(" ");

                fwd_container.insert(color.clone(), count);

                let e = reverse.entry(color).or_insert_with(HashMap::new);
                e.insert(container_color.to_string(), count);
            }
            (reverse, forward)
        },
    ))
}

fn part1(rules: &Rules) -> err::Result<usize> {
    fn collect<'a, 'b>(seen: &'a mut HashSet<&'b str>, rules: &'b Rules, color: &'b str) {
        seen.insert(color);
        match rules.get(color) {
            None => {}
            Some(containers) => containers.keys().for_each(|k| {
                collect(seen, rules, &k);
            }),
        }
    }
    let mut seen = set!(size = 200);
    collect(&mut seen, rules, "shiny gold");
    seen.remove("shiny gold");
    Ok(seen.len())
}

fn part2(rules: &Rules) -> err::Result<usize> {
    fn count(rules: &Rules, color: &str, multiplier: usize) -> usize {
        match rules.get(color) {
            None => 0,
            Some(sub_bags) => sub_bags.iter().fold(0, |acc, (next, next_count)| {
                acc + (multiplier * next_count) + count(rules, &next, multiplier * next_count)
            }),
        }
    }
    Ok(count(rules, "shiny gold", 1))
}

pub fn run() -> err::Result<()> {
    let input = time!(
        file::read("../input/d07.txt")?,
        (ms) -> println!("  -> read[{}ms]", ms),
    );
    let input = time!(
        parse(&input)?,
        (ms) -> println!("  -> parse[{}ms]", ms),
    );

    let (ms, res) = time!(part1(&input.0)?);
    println!("  -> p1[{}ms]: {}", ms, res);
    let (ms, res) = time!(part2(&input.1)?);
    println!("  -> p2[{}ms]: {}", ms, res);

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    static INPUT: &str = r##"
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"##;

    #[test]
    fn test_p1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part1(&input.0).expect("p1 fail"), 4);
    }

    #[test]
    fn test_p2_1() {
        let input = parse(INPUT).expect("parse fail");
        assert_eq!(part2(&input.1).expect("p2 fail"), 32);
    }

    static INPUT_2: &str = r##"
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
"##;

    #[test]
    fn test_p2_2() {
        let input = parse(INPUT_2).expect("parse fail");
        assert_eq!(part2(&input.1).expect("p2 fail"), 126);
    }
}
