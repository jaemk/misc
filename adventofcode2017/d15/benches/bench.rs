#![feature(test)]
extern crate test;
extern crate d15;


use test::Bencher;


#[bench]
fn bench_part1(b: &mut Bencher) {
    b.iter(|| {
        d15::part1(512, 191);
    })
}


#[bench]
fn bench_part1_chan_buf_1_000(b: &mut Bencher) {
    b.iter(|| {
        d15::part1_channels(1000, 512, 191);
    })
}


#[bench]
fn bench_part1_chan_buf_5_000(b: &mut Bencher) {
    b.iter(|| {
        d15::part1_channels(5000, 512, 191);
    })
}


#[bench]
fn bench_part1_chan_buf_10_000(b: &mut Bencher) {
    b.iter(|| {
        d15::part1_channels(10000, 512, 191);
    })
}


#[bench]
fn bench_part1_chan_buf_50_000(b: &mut Bencher) {
    b.iter(|| {
        d15::part1_channels(50000, 512, 191);
    })
}


#[bench]
fn bench_part1_chan_buf_100_000(b: &mut Bencher) {
    b.iter(|| {
        d15::part1_channels(100000, 512, 191);
    })
}

