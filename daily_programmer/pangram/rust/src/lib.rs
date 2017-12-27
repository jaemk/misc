#![feature(test)]
extern crate test;
#[macro_use] extern crate lazy_static;

use test::Bencher;
use std::collections::HashMap;
use std::sync::Mutex;


lazy_static! {
    static ref ALPHA: Mutex<HashMap<char, bool>> = Mutex::new(HashMap::new());
}


fn alphabet() -> HashMap<char, bool> {
    (b'a'..b'z'+1).map(|n| (n as char, false)).collect()
}

pub fn is_pangram(s: &str) -> bool {
    let mut alpha = alphabet();
    for c in s.to_lowercase().chars() {
        alpha.insert(c, true);
    }

    if alpha.iter().any(|(_, v)| !v) { return false; }
    true
}


fn populate() {
    for n in b'a'..b'z'+1 {
        ALPHA.lock().unwrap().insert(n as char, false);
    }
}

pub fn is_pangram_static(s: &str) -> bool {
    populate();
    for c in s.to_lowercase().chars() {
        ALPHA.lock().unwrap().insert(c, true);
    }

    if ALPHA.lock().unwrap().iter().any(|(_, v)| !v) { return false; }
    true
}


pub struct Pangramist {
    map: HashMap<char, bool>,
}
impl Pangramist {
    pub fn new() -> Self {
        Pangramist { map: HashMap::new() }
    }
    pub fn clear(&mut self) {
        for n in b'a'..b'z'+1 {
            self.map.insert(n as char, false);
        }
    }
    pub fn is_pangram(&mut self, s: &str) -> bool {
        for c in s.to_lowercase().chars() {
            self.map.insert(c, true);
        }
        if self.map.iter().any(|(_, v)| !v) { return false; }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static CASES: [(&'static str, bool); 9] = [
        ("", false),
        ("The quick brown fox jumps over the lazy dog", true),
        ("a quick movement of the enemy will jeopardize five gunboats", false),
        ("the quick brown fish jumps over the lazy dog", false),
        ("the 1 quick brown fox jumps over the 2 lazy dogs", true),
        ("7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog", false),
        ("\"Five quacking Zephyrs jolt my wax bed.\"", true),
        ("Victor jagt zwölf Boxkämpfer quer über den großen Sylter Deich.", true),
        ("Широкая электрификация южных губерний даст мощный толчок подъёму сельского хозяйства.", false),
    ];

    #[test]
    fn it_works() {
        for case in CASES.iter() {
            assert_eq!(is_pangram(case.0), case.1, "{:?}", case.0);
        }
    }

    #[bench]
    fn bench_naive(b: &mut Bencher) {
        b.iter(|| {
            for case in CASES.iter() {
                is_pangram(case.0);
            }
        });
    }

    #[bench]
    fn bench_static(b: &mut Bencher) {
        b.iter(|| {
            for case in CASES.iter() {
                is_pangram_static(case.0);
            }
        })
    }

    #[bench]
    fn bench_struct(b: &mut Bencher) {
        let mut p = Pangramist::new();
        b.iter(|| {
            p.clear();
            for case in CASES.iter() {
                p.is_pangram(case.0);
            }
        })
    }
}
