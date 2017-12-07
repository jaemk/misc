extern crate d3;
extern crate env_logger;


const INPUT: u64 = 265149;


pub fn main() {
    init_logger().expect("log init error");

    println!("d3-p1: {}", d3::part1::step_distance(INPUT as u32));
    println!("d3-p2: {}", d3::part2::vec::find_value_larger_than(INPUT));
    println!("d3-p2: {}", d3::part2::hash::find_value_larger_than(INPUT));
    println!("d3-p2: {}", d3::part2::vec::find_value_larger_than(1_000_000_000_000_000));
}



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

