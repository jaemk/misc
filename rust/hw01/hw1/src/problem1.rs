
pub fn sum(slice: &[i32]) -> i32 {
    slice.iter().sum()
}

pub fn dedup(vs: &Vec<i32>) -> Vec<i32> {
    vs.iter().fold(vec![], |mut acc, item| {
        if !acc.contains(item) {
            acc.push(*item);
        }
        acc
    })
}

pub fn filter(vs: &Vec<i32>, pred: &Fn(i32) -> bool) -> Vec<i32> {
    vs.iter().filter(|&i| pred(*i)).cloned().collect::<Vec<_>>()
}
