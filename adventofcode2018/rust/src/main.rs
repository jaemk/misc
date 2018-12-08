#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate chrono;
extern crate regex;
extern crate itertools;

#[macro_use] mod utils;
mod d01;
mod d02;
mod d03;
mod d04;
mod d05;
mod d06;
mod d07;
mod d08;

use std::env;


fn init_logger() {
    use std::io::Write;
    if env::var("LOG").is_err() {
        env::set_var("LOG", "info")
    }
    env_logger::Builder::from_env("LOG")
        .format(|buf, record| {
            write!(buf, "{} [{}] [{}]: {}\n",
                   chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                   record.level(),
                   record.target(),
                   record.args()
            )
        })
        .init();
}

fn main() -> utils::StdResult<()> {
    init_logger();

    info!("**** Advent of code! ****");
    d01::run()?;
    d02::run()?;
    d03::run()?;
    d04::run()?;
    d05::run()?;
    d06::run()?;
    d07::run()?;
    d08::run()?;
    Ok(())
}
