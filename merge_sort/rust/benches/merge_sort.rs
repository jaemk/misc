#![feature(test)]
extern crate test;
extern crate merge_sort;


// cargo +nightly bench

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[bench]
    fn merge_sort(b: &mut Bencher) {
        b.iter(|| {
            let mut v = (0..100000).rev().collect::<Vec<_>>();
            merge_sort::merge_sort(&mut v);
        });
    }

    #[bench]
    fn builtin(b: &mut Bencher) {
        b.iter(|| {
            let mut v = (0..100000).rev().collect::<Vec<_>>();
            v.sort();
        });
    }
}
