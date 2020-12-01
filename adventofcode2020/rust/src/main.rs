#[macro_use]
mod utils;

mod d01;

macro_rules! report {
    ($day:expr, $body:expr) => {{
        let (millis, _) = time!($body);
        println!("Day {} total time: {}ms", $day, millis);
    }};
}

fn run() -> utils::err::Result<()> {
    report!("1", d01::run()?);
    Ok(())
}

fn main() {
    println!("Advent of code 2020!");
    if let Err(e) = run() {
        eprintln!("Error: {:?}", e);
    }
}
