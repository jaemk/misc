#![allow(unused)]

use std::path::Path;
use std::fs::File;
use std::io::{self, Read, BufRead};
use std::collections::HashMap;


pub type StdError = Box<std::error::Error>;
pub type StdResult<T> = Result<T, StdError>;


pub fn load_file<T: AsRef<Path>>(path: T) -> Result<String, StdError> {
    let p = path.as_ref();
    let mut f = File::open(p.clone())
        .map_err(|e| format!("Error loading {:?}: {:?}", p, e))?;
    let mut s = String::new();
    f.read_to_string(&mut s)
        .map_err(|e| format!("Error reading {:?}: {:?}", p, e))?;
    Ok(s.trim_end().into())
}


pub fn file_lines<T: AsRef<Path>>(path: T) -> Result<impl Iterator<Item=io::Result<String>>, StdError> {
    let f = File::open(path)?;
    let r = io::BufReader::new(f);
    Ok(r.lines())
}


pub fn freqs(s: &str) -> HashMap<char, u32> {
    let mut map = HashMap::new();
    for c in s.chars() {
        let e = map.entry(c).or_insert(0);
        *e += 1;
    }
    map
}

