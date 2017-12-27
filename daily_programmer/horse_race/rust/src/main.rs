
/// Bubble slice sort
fn bubble(vals: &mut [u32]) -> u32 {
    let len = vals.len();
    let mut swapped = 0;
    for i in 0..len-1 {
        if vals[i] > vals[i+1] {
            vals.swap(i, i+1);
            swapped += 1;
        }
    }
    swapped
}

fn bubble_slice(vals: &mut [u32]) -> u32 {
    let slice_size = 5;
    let mut n_slices = vals.len() - slice_size + 1;
    let mut count = 0;
    while n_slices > 0 {
        let mut last_n_swapped = 0;
        for i in 0..n_slices {
            let end = i + slice_size;
            last_n_swapped = bubble(&mut vals[i..end as usize]);
            count += 1;
        }
        if last_n_swapped == 0 {
            n_slices -= slice_size;
        } else {
            n_slices -= 1;
        }
    }
    count
}


//-------------------------------------


/// Grouped bucket/merge sort
fn merge_sort_groups(mut sink: Vec<Vec<u32>>, mut groups: Vec<Vec<u32>>, mut count: u32) -> (u32, Vec<u32>) {
    // base
    if groups.len() == 1 {
        return (count, groups[0].to_vec());
    }

    // finished sorting slowest
    if groups.is_empty() {
        let new_groups = sink;
        let new_sink = vec![];
        return merge_sort_groups(new_sink, new_groups, count);
    }
    let split_point = if groups.len() < 5 { groups.len() } else { 5 };
    let rest = groups.split_off(split_point);

    let longest = groups.iter().map(|g| g.len()).max().unwrap();
    for g in groups.iter_mut() {
        let len = g.len();
        if len < longest {
            for _ in 0..(longest-len) {
                g.push(0);
            }
        }
    }
    let mut new_group = vec![];
    let len = groups.len();
    loop {
        let mut min = u32::max_value();
        let mut ind = 0;
        for i in 0..len {
            match groups[i].first() {
                None => continue,
                Some(&v) => {
                    if v < min {
                        min = v;
                        ind = i;
                    }
                }
            }
        }
        if min < u32::max_value() && min > 0 {
            new_group.push(min);
            groups[ind].remove(0);
            count += 1;
        } else {
            break;
        }
    }
    sink.push(new_group);
    return merge_sort_groups(sink, rest, count);
}

fn sliced_merge(vals: Vec<u32>) -> (u32, Vec<u32>) {
    let races = vals.chunks(5).map(|chunk| {
        let mut part = chunk.to_vec();
        part.sort();
        part
    }).collect::<Vec<_>>();
    let count = 5;
    let (count, sorted) = merge_sort_groups(vec![], races, count);
    (count, sorted)
}


//----------------------------------------------


fn main() {
    let mut vals = vec![107,47,102,64,50,100,28,91,27,5,22,114,23,42,13,3,93,8,92,79,53,83,63,7,15,66,105,57,14,65,58,113,112,1,62,103,120,72,111,51,9,36,119,99,30,20,25,84,16,116,98,18,37,108,10,80,101,35,75,39,109,17,38,117,60,46,85,31,41,12,29,26,74,77,21,4,70,61,88,44,49,94,122,2,97,73,69,71,86,45,96,104,89,68,40,6,87,115,54,123,125,90,32,118,52,11,33,106,95,76,19,82,56,121,55,34,24,43,124,81,48,110,78,67,59];
    let count = bubble_slice(&mut vals);
    println!("window sorted in {} steps", count);

    let vals = vec![107,47,102,64,50,100,28,91,27,5,22,114,23,42,13,3,93,8,92,79,53,83,63,7,15,66,105,57,14,65,58,113,112,1,62,103,120,72,111,51,9,36,119,99,30,20,25,84,16,116,98,18,37,108,10,80,101,35,75,39,109,17,38,117,60,46,85,31,41,12,29,26,74,77,21,4,70,61,88,44,49,94,122,2,97,73,69,71,86,45,96,104,89,68,40,6,87,115,54,123,125,90,32,118,52,11,33,106,95,76,19,82,56,121,55,34,24,43,124,81,48,110,78,67,59];
    let (count, _) = sliced_merge(vals);
    println!("merge sorted in {} steps", count);
}
