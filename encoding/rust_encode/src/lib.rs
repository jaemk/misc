extern crate itertools;

use itertools::Itertools;

/// Part 2 -- Extensible byte base
/// -- encode/decode/transform vector repr. to string output
pub type MBArray = Vec<Vec<Vec<u32>>>;

/// Transform into string output
pub fn maxbyte_array_to_string(mb_arr: MBArray) -> String {
    mb_arr.iter().map(|line| {
        line.iter().map(|items| {
            items.iter().map(|item| item.to_string()).join(" ")
        }).join(" ")
    }).join("\n")
}

/// Encode string input into vec representation
pub fn encode_maxbyte_array(info: &str) -> MBArray {
    info.split('\n').map(|line| {
        line.trim().split(' ').map(|val_str| {
            let mut val = val_str.parse::<u32>().unwrap();
            let mut constrained: Vec<u32> = Vec::with_capacity(((val / 255) + 1) as usize);
            while val >= 255 {
                constrained.push(255);
                val -= 255;
            }
            constrained.push(val);
            constrained
        }).collect::<Vec<_>>()
    }).collect::<Vec<_>>()
}

/// Decode string input back to original
pub fn decode_maxbyte_array(info: &str) -> MBArray {
    info.split('\n').map(|line| {
        let mut deco = line.trim().split(' ').fold(vec![0], |mut acc, val_str| {
            let val = val_str.parse::<u32>().unwrap();
            let ind = acc.len() - 1;
            acc[ind] += val;
            if val < 255 {
                acc.push(0);
            }
            acc
        });
        deco.pop();
        vec![deco]
    }).collect::<Vec<_>>()
}
