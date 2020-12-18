use itertools::Itertools;

#[macro_use]
mod utils;

mod d01;
mod d02;
mod d03;
mod d04;
mod d05;
mod d06;
mod d07;
mod d08;
mod d09;
mod d10;
mod d11;
mod d12;
mod d13;
mod d14;
mod d15;
mod d16;
mod d17;

#[inline]
fn ensure_input(day: &str) -> utils::err::Result<bool> {
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
    let resp = ureq::get(&format!("https://adventofcode.com/2020/day/{}/input", day))
        .set("cookie", &format!("session={}", session_cookie))
        .call();
    if let Some(e) = resp.synthetic_error() {
        return Err(format!("request error: {:?}", e).into());
    }

    let mut new_file = std::fs::File::create(input_file)?;
    std::io::copy(&mut resp.into_reader(), &mut new_file)?;

    Ok(false)
}

macro_rules! day {
    ($req_day:expr, $day:expr, $body:expr) => {{
        let should_run = if let Some(req_day) = $req_day {
            req_day == $day
        } else { true };
        if should_run {
            println!("Day {}:", $day);
            time!(ensure_input($day)?,
            (res, ms) ->  {
                if res {
                    println!("  -> input[{}ms] found existing file", ms);
                } else {
                    println!("  -> input[{}ms] retrieved new file", ms);
                }
            });
            let (millis, _) = time!($body);
            println!("time: {}ms\n", millis);
        }
    }};
}

fn run(day: Option<&str>) -> utils::err::Result<()> {
    day!(day, "1", d01::run()?);
    day!(day, "2", d02::run()?);
    day!(day, "3", d03::run()?);
    day!(day, "4", d04::run()?);
    day!(day, "5", d05::run()?);
    day!(day, "6", d06::run()?);
    day!(day, "7", d07::run()?);
    day!(day, "8", d08::run()?);
    day!(day, "9", d09::run()?);
    day!(day, "10", d10::run()?);
    day!(day, "11", d11::run()?);
    day!(day, "12", d12::run()?);
    day!(day, "13", d13::run()?);
    day!(day, "14", d14::run()?);
    day!(day, "15", d15::run()?);
    day!(day, "16", d16::run()?);
    day!(day, "17", d17::run()?);
    Ok(())
}

fn main() {
    println!("Advent of code 2020!\n");
    let args = std::env::args().collect_vec();
    let day = args.get(1).map(|s| s.as_str());
    let (millis, _) = time!({
        if let Err(e) = run(day) {
            eprintln!("Error: {:?}", e);
        }
    });
    println!("total time: {}ms", millis)
}
