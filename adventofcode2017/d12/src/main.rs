/*!
http://adventofcode.com/2017/day/12
*/
use std::collections::{HashSet, HashMap};


static INPUT: &'static str = include_str!("../input.txt");


struct Node {
    _id: usize,
    conns: HashSet<usize>,
}


struct Graph {
    map: HashMap<usize, Node>,
}
impl Graph {
    fn from_input(input: &str) -> Self {
        let map = input.trim()
            .lines()
            .fold(HashMap::new(), |mut acc, line| {
                let mut parts = line.trim().split("<->");
                let id = parts.next()
                    .expect("id missing")
                    .trim()
                    .parse::<usize>()
                    .expect("invalid id int");
                let conns = parts.next()
                    .expect("missing connections")
                    .trim()
                    .split(",")
                    .map(|conn_id| conn_id.trim().parse::<usize>().expect("invalid conn_id int"))
                    .collect::<HashSet<_>>();
                let node = Node { _id: id, conns };
                acc.insert(id, node);
                acc
            });
        Self { map }
    }

    fn walk(&self, connected: &mut HashSet<usize>, node: &Node) {
        let unseen: Vec<usize> = node.conns.difference(connected).cloned().collect();
        for id in &unseen {
            connected.insert(*id);
        }
        for id in &unseen {
            let node = self.map.get(id).expect("missing child node");
            self.walk(connected, &node);
        }
    }

    fn group_for(&self, node_id: &usize) -> HashSet<usize> {
        let mut connected = HashSet::new();
        connected.insert(*node_id);
        let start = self.map.get(node_id).expect(&format!("node `{}` does not exist", node_id));
        self.walk(&mut connected, start);
        connected
    }

    fn id_set(&self) -> HashSet<usize> {
        self.map.keys().cloned().collect()
    }
}


fn part1(input: &str) -> usize {
    let graph = Graph::from_input(input);
    graph.group_for(&0).len()
}


fn part2(input: &str) -> usize {
    let graph = Graph::from_input(input);
    let all_ids = graph.id_set();
    let mut groups: Vec<HashSet<usize>> = vec![];
    for id in &all_ids {
        if groups.iter().any(|group| group.contains(id)) {
            continue
        }
        let new_group = graph.group_for(id);
        groups.push(new_group);
    }
    groups.len()
}


pub fn main() {
    println!("d12-p1: {}", part1(INPUT));
    println!("d12-p2: {}", part2(INPUT));
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &'static str = r##"
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
"##;

    #[test]
    fn p1() {
        assert_eq!(part1(TEST_INPUT), 6);
    }

    #[test]
    fn p2() {
        assert_eq!(part2(TEST_INPUT), 2);
    }
}
