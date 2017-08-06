use std::env;
use std::fs;
use std::collections::HashMap;
use std::io::{BufRead, BufReader};


#[macro_use] mod errors;
use errors::*;


fn run(args: &[String]) -> Result<()> {
    if args.len() < 4 {
        bail!(Error::Msg, "usage: {} filename keyfield-index valuefield-index", args[0]);
    }

    let current_dir = env::current_dir()?;
    let filepath = current_dir.join(&args[1]);
    let keyfield_index = args[2].parse::<usize>()?;
    let valuefield_index = args[3].parse::<usize>()?;
    let max_field_index = std::cmp::max(keyfield_index, valuefield_index);
    let delim = '\t';

    let mut sum_by_key = HashMap::new();

    let f = fs::File::open(filepath)?;
    let mut input = BufReader::new(f);

    let mut line = String::new();

    loop {
        line.clear();
        let n = input.read_line(&mut line)?;
        if n == 0 { break; }

        let fields = line.trim_right().split(delim).collect::<Vec<_>>();
        if fields.len() <= max_field_index { continue; }

        let key = unsafe { fields.get_unchecked(keyfield_index) };
        let value = unsafe { fields.get_unchecked(valuefield_index) };

        let e = sum_by_key.entry(key.to_string()).or_insert(0);
        *e += value.parse::<u32>()?;
    }

    let n = sum_by_key.len();
    if n == 0 {
        println!("No entries.");
    } else {
        let max = sum_by_key.iter().max_by_key(|&(_, v)| v)
            .ok_or_else(|| err_msg!("no max value found...?"))?;
        println!("max_key: {}, max_entry: {}", max.0, max.1);
    }

    Ok(())
}

fn main() {
    let args = env::args().collect::<Vec<_>>();

    if let Err(e) = run(&args) {
        println!("[Error] {}", e);
        ::std::process::exit(1);
    }
}
