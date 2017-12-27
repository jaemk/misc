static S: &'static str = "ngrhhqbhnsipkcoqjyviikvxbxyphsnjpdxkhtadltsuxbfbrkof";

fn run(s: &str) -> char {
    for c in s.chars() {
        let mut count = 0;
        let mut slice = &s[..];
        loop {
            if count > 1 { break }
            let ind = {
                if let Some(ind) = slice.find(c) {
                    count += 1;
                    ind + 1
                } else {
                    break;
                }
            };
            slice = &slice[ind..];
        }
        if count == 1 { return c }
    }
    '_'
}

fn main() {
    println!("{} -> {}", S, run(S));
}
