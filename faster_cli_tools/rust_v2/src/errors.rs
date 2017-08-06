/*!
Error type, conversions, and macros
*/
use std;


pub type Result<T> = std::result::Result<T, Error>;


#[derive(Debug)]
pub enum Error {
    Msg(String),
    Io(std::io::Error),
    ParseInt(std::num::ParseIntError),
}
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        Error::Io(e)
    }
}
impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Error {
        Error::ParseInt(e)
    }
}
impl std::error::Error for Error {
    fn description(&self) -> &str {
        "CLI Error"
    }
    fn cause(&self) -> Option<&std::error::Error> {
        use Error::*;
        Some(match *self {
            Io(ref e) => e,
            ParseInt(ref e) => e,
            _ => return None
        })
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Error::*;
        match *self {
            Msg(ref s)      => write!(f, "{}", s),
            Io(ref e)       => write!(f, "Io: {}", e),
            ParseInt(ref e) => write!(f, "ParseInt: {}", e),
        }
    }
}


macro_rules! format_err {
    ($etype:expr, $literal:expr) => {
        $etype(format!($literal))
    };
    ($etype:expr, $literal:expr, $($arg:expr),*) => {
        $etype(format!($literal, $($arg),*))
    };
}

macro_rules! err_msg {
    ($literal:expr) => {
        format_err!(Error::Msg, $literal)
    };
    ($literal:expr, $($arg:expr),*) => {
        format_err!(Error::Msg, $literal, $($arg),*)
    };
}

macro_rules! bail {
    ($etype:expr, $literal:expr) => {
        return Err(format_err!($etype, $literal))
    };
    ($etype:expr, $literal:expr, $($arg:expr),*) => {
        return Err(format_err!($etype, $literal, $($arg),*))
    };
}
