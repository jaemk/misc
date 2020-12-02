#![allow(unused)]

use std::cmp::Eq;
use std::collections::HashMap;
use std::fs::File;
use std::hash::Hash;
use std::io::{self, BufRead, Read};
use std::path::Path;

pub mod err {
    pub type Error = Box<dyn std::error::Error>;
    pub type Result<T> = std::result::Result<T, Error>;
}

#[macro_use]
pub mod file {
    use super::*;

    pub fn read<T: AsRef<Path>>(path: T) -> Result<String, err::Error> {
        let p = path.as_ref();
        let mut f = File::open(&p).map_err(|e| format!("Error loading {:?}: {:?}", p, e))?;
        let mut s = String::new();
        f.read_to_string(&mut s)
            .map_err(|e| format!("Error reading {:?}: {:?}", p, e))?;
        Ok(s.trim_end().into())
    }

    pub fn lines<T: AsRef<Path>>(
        path: T,
    ) -> Result<impl Iterator<Item = io::Result<String>>, err::Error> {
        let f = File::open(path)?;
        let r = io::BufReader::new(f);
        Ok(r.lines())
    }

    #[macro_export]
    macro_rules! embed_input {
        ($day:expr) => {
            include_str!(concat!("../../../input/", $day))
        };
    }
}

#[macro_export]
macro_rules! time {
    ($body:expr) => {{
        use ::std::time;
        let start = time::Instant::now();
        let res = { $body };
        let elapsed = start.elapsed();
        let millis = (elapsed.as_secs() as f64 * 1000.) + (elapsed.subsec_micros() as f64 / 1000.);
        (millis, res)
    }};
}

#[macro_export]
macro_rules! map {
    () => {
        std::collections::HashMap::new()
    };
    ($($k:expr => $v:expr),* $(,)*) => {
        {
            let mut map = std::collections::HashMap::new();
            $(
                map.insert($k, $v);
            )*
            map
        }
    }
}

#[macro_export]
macro_rules! set {
    () => {
        std::collections::HashSet::new()
    };
    (size=$s:expr) => {
        std::collections::HashSet::with_capacity($s)
    };
    ($($k:expr),* $(,)*) => {
        {
            let mut set = std::collections::HashSet::new();
            $(
                set.insert($k);
            )*
            set
        }
    };
}

pub fn freqs<T: Hash + Eq>(elems: impl Iterator<Item = T>) -> HashMap<T, u32> {
    let mut map = HashMap::new();
    for item in elems {
        let e = map.entry(item).or_insert(0);
        *e += 1;
    }
    map
}
