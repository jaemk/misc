pub enum Part {
    One,
    Two,
}

fn cycle(buf: &mut [char], c: char) {
    let len = buf.len();
    for i in 0..len-1 {
        buf[i] = buf[i+1];
    }
    buf[len-1] = c;
}

fn abba(buf: &[char; 4]) -> bool {
    buf[0] == buf[3] &&
    buf[1] == buf[2] &&
    buf[0] != buf[1]
}

fn one(content: &String) -> u32 {
    let mut count = 0;
    for line in content.trim().split('\n') {
        let mut buf = ['-'; 4];
        let mut ok = false;
        let mut brak = false;
        for c in line.chars() {
            cycle(&mut buf, c);
            if c == '['      { brak = true; }
            else if c == ']' { brak = false; }
            let abba = abba(&buf);
            if abba && brak       { ok = false; break; }
            else if abba && !brak { ok = true; }
        }
        if ok { count += 1; }
    }
    count
}

fn aba(buf: &[char; 3]) -> bool {
    buf[0] == buf[2] && buf[0] != buf[1]
}

fn flip(buf: &mut [char; 3]) {
    buf[0] = buf[1];
    buf[1] = buf[2];
    buf[2] = buf[0];
}

fn two(content: &String) -> u32 {
    let mut count = 0;
    for line in content.trim().split('\n') {
        let mut abas = vec![];
        let mut babs = vec![];

        let mut buf = ['-'; 3];
        let mut brak = false;
        for c in line.chars() {
            cycle(&mut buf, c);
            if c == '['      { brak = true; }
            else if c == ']' { brak = false; }
            let aba = aba(&buf);
            if aba {
                if brak {
                    babs.push(buf.clone());
                } else {
                    abas.push(buf.clone());
                }
            }
        }
        if abas.len() == 0 || babs.len() == 0 {
            continue;
        }
        for buf in abas.iter_mut() {
            flip(buf);
            if babs.contains(buf) {
                count += 1;
                break;
            }
        }
    }
    count
}

pub fn eval(content: &String, part: Part) -> u32 {
    match part {
        Part::One => one(content),
        Part::Two => two(content),
    }
}
