#[macro_use]
mod utils;

mod d01;

fn run() -> utils::err::Result<()> {
    d01::run()?;
    Ok(())
}

fn main() {
    println!("Advent of code 2020!");
    if let Err(e) = run() {
        eprintln!("Error: {:?}", e);
    }
}
