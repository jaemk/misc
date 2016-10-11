extern crate itertools;

use itertools::Itertools;  // for iter().join

pub fn into_fibs(n: i32) -> Vec<i32> {
    // track the last two fib nums up to n
    let mut fibnums: [i32; 2] = [0, 1];
    let mut next = 1;
    while next <= n {
        fibnums[0] = fibnums[1];
        fibnums[1] = next;
        next = fibnums[0] + fibnums[1];
    }
    // save the needed ones
    let mut rem = n;
    let mut fibs = vec![];
    while rem > 0 {
        if fibnums[1] <= rem {
            rem -= fibnums[1];
            fibs.push(fibnums[1]);
        }
        next = fibnums[1];
        fibnums[1] = fibnums[0];
        fibnums[0] = next - fibnums[0];
    }
    fibs
}

pub fn as_sum_of_fibs(n: i32) -> String {
    let fibs = into_fibs(n);
    format!("{} = {}", n, fibs.iter().join(" + ").as_str())
}
