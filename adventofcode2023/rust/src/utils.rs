#![allow(unused)]

use std::cmp::Eq;
use std::collections::HashMap;
use std::fs::File;
use std::hash::Hash;
use std::io::{self, BufRead, Read, Write};
use std::path::Path;

#[macro_use]
pub mod file {
    use super::*;

    pub async fn ensure_input(day: &str) -> crate::Result<bool> {
        let input_dir = std::env::current_dir()?.join("..").join("input");
        if !input_dir.exists() {
            return Err("input dir is missing".into());
        }
        let input_file_name = format!("d{:0>2}.txt", day);
        let input_file = input_dir.join(input_file_name);
        if input_file.exists() && input_file.is_file() {
            return Ok(true);
        }

        println!("  -> retrieving input for day {}", day);
        let session_cookie = std::env::var("SESSION_COOKIE")
            .map_err(|e| format!("missing SESSION_COOKIE env var: {}", e))?;
        let resp = reqwest::Client::new()
            .get(&format!("https://adventofcode.com/2023/day/{}/input", day))
            .header("cookie", &format!("session={}", session_cookie))
            .send()
            .await?;
        let resp = resp.error_for_status()?;
        let resp = resp.text().await?;
        tokio::fs::write(input_file, resp.as_bytes()).await?;

        Ok(false)
    }

    pub async fn read<T: AsRef<Path>>(path: T) -> crate::Result<String> {
        let p = path.as_ref();
        let s = tokio::fs::read_to_string(&path)
            .await
            .map_err(|e| format!("Error reading {:?}: {:?}", p, e))?;
        Ok(s.trim_end().into())
    }

    #[macro_export]
    macro_rules! embed_input {
        ($day:expr) => {
            include_str!(concat!("../../../input/", $day))
        };
    }
}

macro_rules! _millis_since_instant {
    ($inst:expr) => {{
        let elapsed = $inst.elapsed();
        let millis = (elapsed.as_secs() as f64 * 1000.) + (elapsed.subsec_micros() as f64 / 1000.);
        millis
    }};
}

#[macro_export]
macro_rules! time {
    ($body:expr) => {{
        use ::std::time;
        let start = time::Instant::now();
        let res = { $body };
        let millis = _millis_since_instant!(start);
        (millis, res)
    }};
    ($body:expr,
     ($ms:ident) -> $report:expr
     $(,)*) => {{
        use ::std::time;
        let start = time::Instant::now();
        let res = { $body };
        let $ms = _millis_since_instant!(start);
        $report;
        res
    }};
    ($body:expr,
     ($result:ident, $ms:ident) -> $report:expr
     $(,)*) => {{
        use ::std::time;
        let start = time::Instant::now();
        let $result = { $body };
        let $ms = _millis_since_instant!(start);
        $report;
        $result
    }};
}

#[macro_export]
macro_rules! day {
    ($req_day:expr, $day:expr, $body:expr) => {{
        let should_run = if let Some(req_day) = $req_day {
            req_day == $day
        } else { true };
        if should_run {
            println!("Day {}:", $day);
            time!($crate::utils::file::ensure_input($day).await?,
            (res, ms) ->  {
                if res {
                    println!("  -> input[{ms:.3}ms] found existing input file");
                } else {
                    println!("  -> input[{ms:.3}ms] retrieved input file");
                }
            });
            let (millis, _) = time!($body);
            println!("time: {millis:.3}ms\n");
        }
    }};
}

#[macro_export]
macro_rules! map {
    () => {
        std::collections::HashMap::new()
    };
    (size=$s:expr) => {
        std::collections::HashMap::with_capacity($s)
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
