extern crate merge_sort;


fn main() {
    if let Err(e) = run() {
        println!("Error: {}", e);
    }
}


fn run() -> Result<(), Box<::std::error::Error>> {
    let mut stuff = vec![38, 27, 43, 3, 9, 82, 10];
    println!("before: {:?}", stuff);
    merge_sort::merge_sort(&mut stuff);
    println!("sorted: {:?}", stuff);

    let mut strings = ["A", "B", "C", "a", "b", "0", "-"]
        .iter().map(|s| s.to_string()).collect::<Vec<_>>();
    println!("\nbefore: {:?}", strings);
    merge_sort::merge_sort(&mut strings);
    println!("sorted: {:?}", strings);

    let mut chars = vec!['a', 'b', 'c', 'A', 'B', 'C'];
    println!("\nbefore: {:?}", chars);
    merge_sort::merge_sort(&mut chars);
    println!("sorted: {:?}", chars);
    Ok(())
}

