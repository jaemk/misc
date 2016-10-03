#![feature(test)]

extern crate swipes;
extern crate test;

use test::Bencher;

#[bench]
fn bench_load_to_hashmap(b: &mut Bencher) {
    b.iter(|| {
        let mut m = swipes::MatcherHash::new();
        m.load("enable1.txt");
    });
}

#[bench]
fn bench_search_hashmap1(b: &mut Bencher) {
    let mut m = swipes::MatcherHash::new();
    m.load("enable1.txt");
    b.iter(|| {
        m.contained_by("qwertyuytresdftyuioknn");
    });
}

#[bench]
fn bench_search_hashmap2(b: &mut Bencher) {
    let mut m = swipes::MatcherHash::new();
    m.load("enable1.txt");
    b.iter(|| {
        m.contained_by("gijakjthoijerjidsdfnokg");
    });
}

#[bench]
fn bench_read_file_to_vec(b: &mut Bencher) {
    b.iter(|| {
        let mut m = swipes::MatcherVec::new();
        m.load("enable1.txt");
    });
}

#[bench]
fn bench_search_nohash_brute1(b: &mut Bencher) {
    let mut m = swipes::MatcherVec::new();
    m.load("enable1.txt");
    b.iter(|| {
        m.contained_by("qwertyuytresdftyuioknn");
    });
}

#[bench]
fn bench_search_nohash_brute2(b: &mut Bencher) {
    let mut m = swipes::MatcherVec::new();
    m.load("enable1.txt");
    b.iter(|| {
        m.contained_by("gijakjthoijerjidsdfnokg");
    });
}

#[bench]
fn bench_search_raw_from_file1(b: &mut Bencher) {
    b.iter(|| {
        swipes::contained_by_raw("enable1.txt", "gqwertyuytresdftyuioknn");
    });
}

#[bench]
fn bench_search_raw_from_file2(b: &mut Bencher) {
    b.iter(|| {
        swipes::contained_by_raw("enable1.txt", "gijakjthoijerjidsdfnokg");
    });
}

