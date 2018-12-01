#[macro_use] extern crate log;
extern crate env_logger;
extern crate chrono;

mod utils;
mod d01;

use std::env;


fn init_logger() {
    use std::io::Write;
    if env::var("LOG").is_err() {
        env::set_var("LOG", "info")
    }
    env_logger::Builder::from_env("LOG")
        .format(|buf, record| {
            write!(buf, "{} [{}] [{}::{}]: {}\n",
                   chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                   record.level(),
                   record.target(),
                   record.line().unwrap_or(0),
                   record.args()
            )
        })
        .init();
}

fn main() -> utils::StdResult<()> {
    init_logger();

    info!("**** Advent of code! ****");
    d01::run()?;
    Ok(())
}
