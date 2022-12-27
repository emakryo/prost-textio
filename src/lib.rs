//! Pure rust implementation of protocol buffer text format parser.
//! This library is built on [prost_reflect](https://crates.io/crates/prost-reflect).
//!
//! # Usage
//!
//! Example protobuf definition: `src/example.proto`
//! ```protobuf
//! syntax = "proto3";
//!
//! package example;
//!
//! // Sample message
//! mesasge Sample {
//!     int32 a = 1;
//!     string b = 2;
//! }
//! ```
//!
//! Build script to compile into `prost-reflect`-compatible code: `build.rs`
//! ```no_run
//! use prost_reflect_build::Builder;
//!
//! fn main() -> std::io::Result<()> {
//!     let file_descriptor_set_path = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap())
//!         .join("file_descriptor_set.bin");
//!
//!     Builder::new()
//!         .file_descriptor_set_path(&file_descriptor_set_path)
//!         .compile_protos(
//!             &["src/example.proto"],
//!             &["src"],
//!         )
//! }
//! ```
//!
//! Parse text format in `main()` : `src/main.rs`
//! ```ignore
//! use once_cell::sync::Lazy;
//! use prost_reflect::{DynamicMessage, FileDescriptor};
//! use prost::Message;
//!
//! // File descriptor used by prost-reflect DynamicMessage.
//! pub(crate) static FILE_DESCRIPTOR: Lazy<FileDescriptor> = Lazy::new(|| {
//!     FileDescriptor::new(
//!         prost_types::FileDescriptorSet::decode(
//!             include_bytes!(concat!(env!("OUT_DIR"), "/file_descriptor_set.bin")).as_ref(),
//!         )
//!         .unwrap(),
//!     )
//!     .unwrap()
//! });
//!
//! // Include compiled protobuf definition.
//! pub(crate) mod example {
//!     include!(concat!(env!("OUT_DIR"), "/example.rs"));
//! }
//!
//! fn main () {
//!     // Parse from str
//!     let mut proto = DynamicMessage::new(example::Sample::default().descriptor());
//!     prost_textio::parse_from_str(r#"
//!         a: 1
//!         b: "foo"
//!     "#, &mut proto).unwrap();
//!     assert_eq!(proto.a, 1);
//!     assert_eq!(&proto.b, "foo");
//! }
//! ```
//!

pub mod parser;
mod tokenizer;
pub mod printer;
pub mod finder;

#[cfg(test)]
mod test_util;

#[cfg(test)]
use test_util::FILE_DESCRIPTOR;

use thiserror::Error as ThisError;

/// Public error type.
#[derive(Debug, ThisError)]
pub enum Error {
    #[error("IO Error")]
    Io(#[from] std::io::Error),
    #[error("Error during parsing")]
    Parse {
        line: usize,
        column: usize,
        err: ParseError,
    },
    #[error("Error during tokenization")]
    Tokenize { err: TokenizeError },
    #[error("Unhandled error")]
    Todo(#[from] anyhow::Error),
}

/// Error in tokenization.
#[derive(Debug, ThisError)]
pub enum TokenizeError {
    #[error("Non UTF-8 text: {}", .0)]
    NonUtf8(#[from] std::str::Utf8Error),
    #[error("Integer range exceeded: {}", .0)]
    IntegerRangeExceeded(u64),
    #[error("Parse integer: {}", .0)]
    ParseInteger(#[from] std::num::ParseIntError),
    #[error("Parse double: {}", .0)]
    ParseFloat(#[from] std::num::ParseFloatError),
}

/// Error in parsing.
#[derive(Debug, ThisError)]
pub enum ParseError {
    #[error("Message missing required fields: {}", .0.join(","))]
    Uninitialized(Vec<String>),
    #[error("Remaining tokens")]
    RemainingToken,
    #[error("Message is too deep, the parser exceeded the configured recursion limit of {}.", .0)]
    ExceedRecursionLimit(usize),
    #[error("Non repeated Any specified multiple times")]
    DuplicatedAny,
    #[error("Unknown type of Any")]
    UnknownAny,
    #[error("Extension \"{}\" is not defined or is not an extension of \"{}\".", .0, .1)]
    UnknownExtension(String, String),
    #[error("Message type \"{}\" has no field named \"{}\".", .0, .1)]
    UnknownField(String, String),
    #[error("Non-repeated field \"{}\" is specified multiple times.", .0)]
    DuplicatedField(String),
    #[error("Field '{}' is specified along with field '{}', another member of oneof '{}'.", .0, .1, .2)]
    DuplicaedOneOf(String, String, String),
    #[error("Failed to decode: {}", .0)]
    MessageDecode(#[from] prost::DecodeError),
    #[error("Invalid value for boolean field \"{}\". Value: \"{}\".", .0, .1)]
    InvalidBool(String, String),
    #[error("Expected integer or identifier, got: {}", .0)]
    InvalidEnum(String),
    #[error("Unknown enumeration value of \"{}\" for field \"{}\".", .0, .1)]
    UnknwonEnumValue(String, String),
    #[error("Invalid field value: {}", .0)]
    InvalidFieldValue(String),
    #[error("Expected identifier, got: {}", .0)]
    InvalidIdentifier(String),
    #[error("Expected string, got: {}", .0)]
    InvalidString(String),
    #[error("Expected integer, got: {}", .0)]
    InvalidUnsignedInteger(String),
    #[error("Expected decimal number, got {}", .0)]
    NonDecimalAsDouble(String),
    #[error("Expected double, got: {}", .0)]
    InvalidDouble(String),
    #[error("Value of type \"{}\" stored in google.protobuf.Any has missing required fields", .0)]
    MissingInAny(String),
    #[error("Expected \"{}\", found \"{}\".", .0, .1)]
    InvalidToken(String, String),
    #[error("Integer out of range ({})", .0)]
    IntegerRangeExceeded(String),
}

/// Convenient alias of `std::result::Result<T, Error>`
pub type Result<T> = std::result::Result<T, Error>;
