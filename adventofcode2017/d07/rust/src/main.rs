#[macro_use] extern crate log;
extern crate env_logger;

use std::collections::{HashSet, HashMap};


static INPUT: &'static str = include_str!("../../input.txt");


mod part1 {
    use super::*;

    pub fn solve(input: &str) -> &str {
        let mut all = HashSet::new();
        let mut children = HashSet::new();
        for line in input.trim().lines() {
            let mut parts = line.split("->");
            let disc_info = parts.next().expect("There must be a disc name");
            let disc_name = disc_info.split_whitespace().next().unwrap();
            all.insert(disc_name);
            if let Some(child_discs) = parts.next() {
                for child in child_discs.trim().split(",") {
                    children.insert(child.trim());
                }
            }
        }
        all.difference(&children).next().expect("Missing base disc")
    }
}


mod part2 {
    use super::*;

    #[derive(Debug)]
    struct Disc<'a> {
        name: &'a str,
        weight: u64,
        child_names: Option<Vec<&'a str>>,
    }

    /// Hold either the sum of all child weights, or the "fixed"
    /// weight that's required to balance the tower of discs
    enum Either {
        Sum(u64),
        Fix(u64),
    }

    /// Takes a slice of (disc-name, weight) and returns the
    /// frequencies of disc weights sorted least to greatest by occurrence
    /// -- returning Vec<(weight, freq-count)>
    fn frequency(values: &[(&str, u64)]) -> Vec<(u64, usize)> {
        let mut map = HashMap::with_capacity(values.len());
        for &(_, val) in values {
            let e = map.entry(val).or_insert(0);
            *e += 1;
        }
        let mut pairs = map.into_iter().collect::<Vec<_>>();
        pairs.sort_by(|a, b| a.1.cmp(&b.1));
        pairs
    }

    /// Starting from the base disc, walks up the disc-tower and returns
    /// either the sum of all discs above it or the "fixed" value that
    /// needs to exist to balance the tower.
    fn check_tree(disc: &Disc, disc_map: &HashMap<&str, Disc>) -> Either {
        if let Some(ref children) = disc.child_names {
            let mut child_weights = Vec::with_capacity(children.len());
            for child in children.iter() {
                let child_disc = disc_map.get(child).expect("missing child");
                match check_tree(child_disc, disc_map) {
                    Either::Fix(fixed_value) => return Either::Fix(fixed_value),
                    Either::Sum(child_weight) => {
                        child_weights.push((child_disc.name, child_weight));
                    }
                }
            }
            debug!("child-weights: {:?}", child_weights);

            // Children themselves are balanced, check if we're balanced.
            // We'll only be unbalanced if we have more than 2 children
            if child_weights.len() > 2 {
                let freq = frequency(&child_weights);
                if freq.len() > 1 {
                    debug!("found an imbalance: {:?}", freq);
                    // [(weight, freq-count), ...]
                    // the first item will be the value that's the odd one out
                    let bad_weight = freq[0].0;
                    let ok_weight = freq[1].0;

                    // find the child name that has this bad weight
                    let mut bad_child_name = None;
                    for &(name, weight) in &child_weights {
                        if weight == bad_weight {
                            bad_child_name = Some(name);
                            break;
                        }
                    }
                    let bad_child_name = bad_child_name.expect("couldn't find child name for bad weight");

                    let bad_child = disc_map.get(bad_child_name).expect("missing bad child disc");
                    return if bad_weight > ok_weight {
                        let diff = bad_weight - ok_weight;
                        Either::Fix(bad_child.weight - diff)
                    } else {
                        let diff = ok_weight - bad_weight;
                        Either::Fix(bad_child.weight + diff)
                    };
                }
            }
            // we're balanced
            let child_sum = child_weights.iter().map(|&(_, weight)| weight).sum::<u64>();
            return Either::Sum(disc.weight + child_sum);
        } else {
            // leaf node
            return Either::Sum(disc.weight);
        }
    }

    pub fn solve(input: &str) -> u64 {
        let mut all_names = HashSet::new();
        let mut child_names = HashSet::new();

        let mut discs: HashMap<&str, Disc> = HashMap::new();

        for line in input.trim().lines() {
            let mut parts = line.split("->");
            let mut disc_info = parts.next().expect("There must be disc info").split_whitespace();
            let disc_name = disc_info.next().expect("There must be a name");
            all_names.insert(disc_name);

            let disc_weight = disc_info.next()
                .expect("There must be a weight")
                .trim_left_matches("(")
                .trim_right_matches(")")
                .parse::<u64>()
                .expect("Weight is an invalid int");

            let mut disc_children = vec![];
            if let Some(child_discs) = parts.next() {
                for child in child_discs.trim().split(",") {
                    let child_name = child.trim();
                    child_names.insert(child_name);
                    disc_children.push(child_name);
                }
            }
            let disc_children = if disc_children.is_empty() { None } else { Some(disc_children) };

            discs.insert(disc_name, Disc { name: disc_name, weight: disc_weight, child_names: disc_children });
        }
        let base_disc_name = all_names.difference(&child_names).next().expect("There must be a base disc");
        let base_disc = discs.get(base_disc_name).expect("missing base node");
        debug!("base_disc: {:?}", base_disc);

        match check_tree(base_disc, &discs) {
            Either::Fix(fixed_value) => fixed_value,
            _ => panic!("Failed balancing discs"),
        }
    }
}


pub fn main() {
    init_logger();
    println!("d7-p1: {}", part1::solve(INPUT));
    println!("d7-p2: {}", part2::solve(INPUT));
}


/// Run with `LOG=debug cargo run` to see debug info
fn init_logger() {
    env_logger::LogBuilder::new()
        .parse(&::std::env::var("LOG").unwrap_or_default())
        .init()
        .expect("Failing initializing logger");
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &'static str = r##"
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"##;

    #[test]
    fn p1() {
        assert_eq!(part1::solve(TEST_INPUT), "tknk", "Expected to find base: `tknk`");
    }

    #[test]
    fn p2() {
        assert_eq!(part2::solve(TEST_INPUT), 60);
    }
}
