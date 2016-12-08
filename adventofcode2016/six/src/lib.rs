use std::collections::HashMap;


pub enum Part {
    One,
    Two,
}

pub fn eval(content: &String, part: Part) -> String {
    let mut lines = content.trim().split('\n');
    let first = lines.next().unwrap();
    let n = first.len();
    let mut store = Vec::with_capacity(n);
    for c in first.chars() {
        let mut map = HashMap::new();
        map.insert(c, 1);
        store.push(map);
    }

    for line in lines {
        for (i, c) in line.chars().enumerate() {
            let ref mut s = store[i];
            *s.entry(c).or_insert(0) += 1
        }
    }

    store.iter().map(|map| {
                let mut group = map.iter().collect::<Vec<_>>();
                match part {
                    Part::One => group.sort_by(|a, b| b.1.cmp(a.1)),
                    Part::Two => group.sort_by(|a, b| a.1.cmp(b.1)),
                };
                group.first().unwrap().0.clone()
    }).collect::<String>()
}
