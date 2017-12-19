extern crate d18;
extern crate env_logger;


static INPUT: &'static str = include_str!("../input.txt");


/// Run with `LOG=debug cargo run` to see debug info
fn init_logger() -> Result<(), Box<std::error::Error>> {
    env_logger::LogBuilder::new()
        .format(|record| {
            format!("[{}] - [{}] -> {}",
                record.level(),
                record.location().module_path(),
                record.args()
                )
            })
        .parse(&::std::env::var("LOG").unwrap_or_default())
        .init()?;
    Ok(())
}


fn run() -> Result<(), Box<std::error::Error>> {
    init_logger()?;
    println!("d18-p1: {}", d18::part1(INPUT)?);
    println!("d18-p2: {}", d18::part2(INPUT)?);
    Ok(())
}


pub fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

