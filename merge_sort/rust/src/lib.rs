use std::cmp::{Ord, Ordering};
use std::mem;


pub fn merge_sort<T: Ord + Clone>(slice: &mut [T]) {
    if slice.len() <= 1 { return; }
    let mid = slice.len() / 2;
    {
        let (mut left, mut right) = slice.split_at_mut(mid);
        merge_sort(&mut left);
        merge_sort(&mut right);
    }
    merge(slice, mid);
}


fn merge<T: Ord + Clone>(slice: &mut [T], mid: usize) {
    let mut left  = slice.to_vec();
    let mut right = left.split_off(mid);

    let mut left_i = 0;
    let mut right_i = 0;
    let mut i = 0;
    while let Some(slice_pos) = slice.get_mut(i) {
        match (left.get_mut(left_i), right.get_mut(right_i)) {
            (None, None) => break,
            (Some(v), None) => {
                mem::swap(slice_pos, v);
                left_i += 1;
            }
            (None, Some(v)) => {
                mem::swap(slice_pos, v);
                right_i += 1;
            }
            (Some(left_v), Some(right_v)) => {
                use Ordering::*;
                match left_v.cmp(&right_v) {
                    Less | Equal => {
                        mem::swap(slice_pos, left_v);
                        left_i += 1;
                    }
                    Greater => {
                        mem::swap(slice_pos, right_v);
                        right_i += 1;
                    }
                }
            }
        }
        i += 1;
    }
}
