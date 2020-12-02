#[macro_use]
mod utils;

mod d01;
mod d02;

macro_rules! report {
    ($day:expr, $body:expr) => {{
        println!("Day {}:", $day);
        let (millis, _) = time!($body);
        println!("time: {}ms\n", millis);
    }};
}

fn run() -> utils::err::Result<()> {
    report!("1", d01::run()?);
    report!("2", d02::run()?);
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
