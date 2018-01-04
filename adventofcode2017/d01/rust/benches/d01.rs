#![feature(test)]

extern crate test;
extern crate d01;

use test::Bencher;


static INPUT: &'static str = include_str!("../../input.txt");


#[bench]
fn bench_part1_chars_copy(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1(INPUT);
    });
}


#[bench]
fn bench_part1_chars_nocopy(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1_nocopy(INPUT);
    });
}


#[bench]
fn bench_part1_chars_nocopy_indexchain(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1_indexchain(INPUT);
    });
}


#[bench]
fn bench_part1_bytes_copy(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1_bytes(INPUT);
    });
}


#[bench]
fn bench_part1_bytes_copy_nocharcast(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1_bytes_nocast(INPUT);
    });
}


#[bench]
fn bench_part1_bytes_nocopy(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1_bytes(INPUT);
    });
}


#[bench]
fn bench_part1_bytes_nocopy_indexchain(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1_bytes_indexchain(INPUT);
    });
}


#[bench]
fn bench_part1_bytes_nocopy_iterator(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1_bytes_nocopy_iterator(INPUT);
    });
}


#[bench]
fn bench_part1_bytes_nocopy_iterator_nocharcast(b: &mut Bencher) {
    b.iter(|| {
        d01::solve1_bytes_nocopy_iterator_nocharcast(INPUT);
    });
}



#[bench]
fn bench_part2_chars_modulo(b: &mut Bencher) {
    b.iter(|| {
        d01::solve2_modulo(INPUT);
    });
}


#[bench]
fn bench_part2_chars_nomodulo(b: &mut Bencher) {
    b.iter(|| {
        d01::solve2(INPUT);
    });
}


#[bench]
fn bench_part2_bytes_modulo(b: &mut Bencher) {
    b.iter(|| {
        d01::solve2_bytes_modulo(INPUT);
    });
}


#[bench]
fn bench_part2_bytes_nomodulo(b: &mut Bencher) {
    b.iter(|| {
        d01::solve2_bytes(INPUT);
    });
}

