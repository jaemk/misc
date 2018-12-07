#![allow(unused)]

use std::path::Path;
use std::fs::File;
use std::hash::Hash;
use std::cmp::Eq;
use std::io::{self, Read, BufRead};
use std::collections::HashMap;


pub type StdError = Box<std::error::Error>;
pub type StdResult<T> = Result<T, StdError>;


#[macro_use]
pub mod file {
    use super::*;

    pub fn load<T: AsRef<Path>>(path: T) -> Result<String, StdError> {
        let p = path.as_ref();
        let mut f = File::open(p.clone())
            .map_err(|e| format!("Error loading {:?}: {:?}", p, e))?;
        let mut s = String::new();
        f.read_to_string(&mut s)
            .map_err(|e| format!("Error reading {:?}: {:?}", p, e))?;
        Ok(s.trim_end().into())
    }


    pub fn lines<T: AsRef<Path>>(path: T) -> Result<impl Iterator<Item=io::Result<String>>, StdError> {
        let f = File::open(path)?;
        let r = io::BufReader::new(f);
        Ok(r.lines())
    }

    #[macro_export]
    macro_rules! input_file {
        ($day:expr) => {
            include_str!(concat!("../../../input/", $day))
        }
    }
}


#[macro_export]
macro_rules! time {
    ($body:expr) => {
        {
            use ::std::time;
            let start = time::Instant::now();
            let res = {
                $body
            };
            let elapsed = start.elapsed();
            let millis = (elapsed.as_secs() as f64 * 1000.) + (elapsed.subsec_micros() as f64 / 1000.);
            (millis, res)
        }
    }
}


#[macro_export]
macro_rules! map {
    ($($k:expr => $v:expr),* $(,)*) => {
        {
            let mut map = HashMap::new();
            $(
                map.insert($k, $v);
            )*
            map
        }
    }
}


#[macro_export]
macro_rules! set {
    ($($k:expr),* $(,)*) => {
        {
            let mut set = HashSet::new();
            $(
                set.insert($k);
            )*
            set
        }
    }
}


pub fn freqs<T: Hash + Eq>(elems: impl Iterator<Item=T>) -> HashMap<T, u32> {
    let mut map = HashMap::new();
    for item in elems {
        let e = map.entry(item).or_insert(0);
        *e += 1;
    }
    map
}

