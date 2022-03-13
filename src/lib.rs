mod parser;
mod serde;
mod tokenizer;

#[cfg(test)]
mod test_util;

#[cfg(test)]
use test_util::FILE_DESCRIPTOR;

pub use parser::*;

#[derive(Debug)]
pub enum ProtoError {
    Io(std::io::Error),
    Parse {
        line: usize,
        column: usize,
        message: String,
    },
    Tokenize {
        line: usize,
        column: usize,
        message: String,
    },
    Todo(String),
    Eof,
}
type Result<T> = std::result::Result<T, ProtoError>;
