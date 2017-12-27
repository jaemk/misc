#![feature(test)]
extern crate test;
extern crate d03;

use test::Bencher;
use d03::part2;

#[bench]
fn bench_part2_vecfield(b: &mut Bencher) {
    b.iter(|| {
        part2::vec::find_value_larger_than(265149);
    });
}

#[bench]
fn bench_part2_vecfield_large(b: &mut Bencher) {
    b.iter(|| {
        part2::vec::find_value_larger_than(5_000_000_000);
    });
}

#[bench]
fn bench_part2_vecfield_very_large(b: &mut Bencher) {
    b.iter(|| {
        part2::vec::find_value_larger_than(1_000_000_000_000_000);
    });
}


#[bench]
fn bench_part2_hash(b: &mut Bencher) {
    b.iter(|| {
        part2::hash::find_value_larger_than(265149);
    });
}

#[bench]
fn bench_part2_hash_large(b: &mut Bencher) {
    b.iter(|| {
        part2::hash::find_value_larger_than(5_000_000_000);
    });
}

#[bench]
fn bench_part2_hash_very_large(b: &mut Bencher) {
    b.iter(|| {
        part2::hash::find_value_larger_than(1_000_000_000_000_000);
    });
}

