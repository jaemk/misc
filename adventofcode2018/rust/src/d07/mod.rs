use std::collections::{HashMap, HashSet};
use regex::Regex;
use lazy_static::lazy_static;
use crate::utils::{StdResult, StdError};


type Deps = HashMap<char, HashSet<char>>;

pub struct DepTree {
    roots: HashSet<char>,
    fwd_deps: Deps,
    back_deps: Deps,
}
impl std::str::FromStr for DepTree {
    type Err = StdError;
    fn from_str(s: &str) -> StdResult<Self> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^Step\s(?P<node>[A-Z]).*before step\s(?P<parent>[A-Z])").unwrap();
        }
        let mut nodes = set!();
        let mut parents = set!();
        let mut fwd_deps = map!();
        let mut back_deps = map!();
        for line in s.trim().lines() {
            let caps = RE.captures(line).ok_or_else(|| "No captures")?;
            let node = caps.name("node")
                .ok_or_else(|| "No node found")?
                .as_str()
                .chars()
                .next()
                .unwrap();
            let parent = caps.name("parent")
                .ok_or_else(|| "No parent found")?
                .as_str()
                .chars()
                .next()
                .unwrap();
            nodes.insert(node);
            nodes.insert(parent);
            parents.insert(parent);

            let e = fwd_deps.entry(node).or_insert(set!());
            e.insert(parent);

            let e = back_deps.entry(parent).or_insert(set!());
            e.insert(node);
        }
        let roots = nodes
            .difference(&parents)
            .cloned()
            .collect::<HashSet<_>>();
        Ok(Self {
            roots,
            fwd_deps,
            back_deps,
        })
    }
}


mod part_1 {
    use super::*;

    pub fn solve(tree: &DepTree) -> StdResult<String> {
        let mut order = vec![];
        let mut seen = set!();
        let mut avail = tree.roots.clone();
        let empty = set!();
        loop {
            if avail.is_empty() { return Ok(order.iter().collect()) }
            let next = *avail
                .iter()
                .filter(|&c| {
                    tree.back_deps
                        .get(c)
                        .unwrap_or(&empty)
                        .difference(&seen)
                        .next()
                        .is_none()
                })
                .min()
                .expect("can't be empty");
            order.push(next);
            seen.insert(next);
            avail.remove(&next);
            for fwd in tree.fwd_deps.get(&next).unwrap_or(&empty) {
                avail.insert(*fwd);
            }
        }
    }
}


mod part_2 {
    use super::*;

    trait ToSeconds {
        fn to_secs(&self) -> usize;
    }

    impl ToSeconds for char {
        fn to_secs(&self) -> usize {
            (*self as u8 - 'A' as u8) as usize + 1
        }
    }


    struct TaskPool {
        work_constant: usize,
        workers: usize,
        inner: HashMap<usize, Vec<char>>,
    }

    impl TaskPool {
        fn new(work_constant: usize, workers: usize) -> Self {
            Self {
                work_constant,
                workers,
                inner: HashMap::with_capacity(workers),
            }
        }

        fn is_full(&self) -> bool {
            self.inner.len() >= self.workers
        }

        fn is_empty(&self) -> bool {
            self.inner.is_empty()
        }

        fn start_step(&mut self, step: char, start_time: usize) {
            let compl_time = step.to_secs() + self.work_constant + start_time;
            let e = self.inner.entry(compl_time).or_insert(vec![]);
            e.push(step);
        }

        fn poll_workers(&mut self, current_time: usize) -> Vec<char> {
            self.inner.remove(&current_time).unwrap_or(vec![])
        }
    }


    pub fn solve(tree: &DepTree, work_constant: usize, workers: usize) -> StdResult<usize> {
        let mut seconds = 0;
        let mut pool = TaskPool::new(work_constant, workers);

        let mut order = vec![];
        let mut seen = set!();
        let mut avail = tree.roots.clone();
        let empty = set!();
        loop {
            let completed = pool.poll_workers(seconds);
            for done in &completed {
                order.push(*done);
                seen.insert(*done);
                for fwd in tree.fwd_deps.get(&done).unwrap_or(&empty) {
                    avail.insert(*fwd);
                }
            }

            if avail.is_empty() && pool.is_empty() { return Ok(seconds) }
            let mut possibles = avail
                .iter()
                .filter(|&c| {
                    tree.back_deps
                        .get(c)
                        .unwrap_or(&empty)
                        .difference(&seen)
                        .next()
                        .is_none()
                })
                .cloned()
                .collect::<Vec<_>>();
            possibles.sort();
            possibles.reverse();

            while !pool.is_full() && !possibles.is_empty() {
                let step = possibles.pop().unwrap();
                pool.start_step(step, seconds);
                avail.remove(&step);
            }
            seconds += 1;
        }
    }
}


pub fn run() -> StdResult<()> {
    info!("*** Day 7 ***");
    let input = input_file!("d07.txt");
    let (ms_parse, tree) = time!({ input.parse::<DepTree>()? });
    let (ms1, res1) = time!({ part_1::solve(&tree)? });
    info!("p1: {}", res1);

    let (ms2, res2) = time!({ part_2::solve(&tree, 60, 5)? });
    info!("p2: {}", res2);

    info!("[Day 7 runtimes] p1: {}ms, p2: {}ms, parsing: {}ms\n", ms1, ms2, ms_parse);
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    static INPUT_1: &'static str = r##"
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
Step F must be finished before step E can begin.
Step H must be finished before step Q can begin.
Step Q must be finished before step D can begin.
"##;
    //    -->A--->B--
    //    /     \     \
    //   C  H-Q---->D----->E
    //    \           /
    //    ---->F-----

    #[test]
    fn test_part_1() {
        let tree = INPUT_1.parse::<DepTree>().unwrap();
        let res = part_1::solve(&tree).unwrap();
        assert_eq!(res, "CABFHQDE");
    }

    static INPUT_2: &'static str = r##"
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
"##;

    #[test]
    fn test_part_2() {
        let tree = INPUT_2.parse::<DepTree>().unwrap();
        let time = part_2::solve(&tree, 0, 2).unwrap();
        assert_eq!(time, 15);
    }
}
