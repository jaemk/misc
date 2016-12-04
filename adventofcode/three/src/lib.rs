pub enum Part {
    One,
    Two,
}

fn one(content: &String) -> u32 {
    content.trim().split('\n').fold(0, |acc, line| {
        let mut sides = line.trim().split_whitespace()
                        .map(|n| n.parse::<u32>().unwrap())
                        .collect::<Vec<_>>();
        sides.sort();
        if sides[0] + sides[1] > sides[2] {
            acc + 1
        } else {
            acc
        }
    })
}

fn two(content: &String) -> u32 {
    let lines = content.trim().split('\n').map(|line| {
        line.trim().split_whitespace()
            .map(|n| n.parse::<u32>().unwrap())
            .collect::<Vec<_>>()
    }).collect::<Vec<_>>();

    let mut a = Vec::with_capacity(lines.len());
    let mut b = Vec::with_capacity(lines.len());
    let mut c = Vec::with_capacity(lines.len());
    for line in lines.iter() {
        a.push(line[0]);
        b.push(line[1]);
        c.push(line[2]);
    }

    let mut n_valid = 0;
    let mut tri = Vec::with_capacity(3);
    for n in a.iter().chain(b.iter()).chain(c.iter()) {
        if tri.len() < 3 {
            tri.push(n);
        } else {
            tri.sort();
            if tri[0] + tri[1] > *tri[2] { n_valid += 1; }
            tri.clear();
            tri.push(n);
        }
    }
    tri.sort();
    if tri[0] + tri[1] > *tri[2] { n_valid += 1; }
    n_valid
}

pub fn eval(content: &String, part: Part) -> u32 {
    match part {
        Part::One => one(content),
        Part::Two => two(content),
    }
}
