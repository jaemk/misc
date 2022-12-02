#[macro_use]
mod utils;
mod d01;
mod d02;

type Error = Box<dyn std::error::Error>;
type Result<T> = std::result::Result<T, Error>;

async fn run(day: Option<&str>) -> Result<()> {
    day!(day, "1", d01::run().await?);
    day!(day, "2", d02::run().await?);
    Ok(())
}

#[tokio::main]
async fn main() {
    let envpath = std::env::current_dir()
        .expect("failed getting current dir")
        .join("..")
        .join(".env.local");
    dotenv::from_path(envpath).ok();
    println!("Advent of code 2022!\n");
    let args = std::env::args().collect::<Vec<_>>();
    let day = args.get(1).map(|s| s.as_str());
    let (millis, _) = time!({
        if let Err(e) = run(day).await {
            eprintln!("Error: {:?}", e);
        }
    });
    println!("total time: {millis:.3}ms");
}
