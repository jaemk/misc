use std::path::Path;
use std::fs::File;
use std::io::Read;


pub type StdError = Box<std::error::Error>;
pub type StdResult<T> = Result<T, StdError>;


pub fn load_file<T: AsRef<Path>>(path: T) -> Result<String, StdError> {
    let p = path.as_ref();
    let mut f = File::open(p.clone())
        .map_err(|e| format!("Error loading {:?}: {:?}", p, e))?;
    let mut s = String::new();
    f.read_to_string(&mut s)
        .map_err(|e| format!("Error reading {:?}: {:?}", p, e))?;
    Ok(s.trim_end().into())
}

