
fn take_until(char_iter: &mut Iterator<Item=char>,
              end_c: Option<char>, n_items: Option<u32>) -> String {
    let mut count = 0;
    let mut group = String::new();
    while let Some(c) = char_iter.next() {
        if end_c.is_some() && end_c.unwrap() == c { break; }
        group.push(c);
        count += 1;
        if n_items.is_some() && n_items.unwrap() == count { break; }
    }
    group
}

pub fn decompress(content: &str) -> usize {
    let mut chars = content.chars().filter(|c| !c.is_whitespace());
    let mut count = 0;
    loop {
        let c = match chars.next() {
            Some(c) => c,
            None => break,
        };
        if c == '(' {
            let marker = take_until(&mut chars, Some(')'), None);
            let mut marker_parts = marker.split('x');
            let n_chars = marker_parts.next().unwrap().parse::<u32>().unwrap();
            let n_times = marker_parts.next().unwrap().parse::<u32>().unwrap();
            let group = take_until(&mut chars, None, Some(n_chars));
            for _ in 0..n_times { count += group.len(); }
        } else {
            count += 1;
        }
    }
    count
}

pub fn recursive_decompression(content: &str) -> usize {
    0
}
