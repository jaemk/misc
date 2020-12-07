#[macro_use]
mod utils;

mod d01;
mod d02;
mod d03;
mod d04;
mod d05;
mod d06;
mod d07;

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
    ($day:expr, $body:expr) => {{
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
    }};
}

fn run() -> utils::err::Result<()> {
    day!("1", d01::run()?);
    day!("2", d02::run()?);
    day!("3", d03::run()?);
    day!("4", d04::run()?);
    day!("5", d05::run()?);
    day!("6", d06::run()?);
    day!("7", d07::run()?);
    Ok(())
}

fn main() {
    println!("Advent of code 2020!\n");
    let (millis, _) = time!({
        if let Err(e) = run() {
            eprintln!("Error: {:?}", e);
        }
    });
    println!("total time: {}ms", millis)
}
