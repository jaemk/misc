extern crate fibrep;

static NUMS: [i32; 8] = [
    4, 100, 30,
    120, 34, 88, 90, 320,
];

pub fn main() {
    for &n in NUMS.iter() {
        println!("{:?}", fibrep::as_sum_of_fibs(n));
    }
}
