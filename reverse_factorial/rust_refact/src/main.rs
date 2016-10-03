extern crate refact;

static VALS: [i32; 4] = [3628800, 479001600, 6, 18];

pub fn main() {
    for v in VALS.iter() {
        println!("{}", refact::reverse_factorial(v));
    }
}

