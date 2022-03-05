mod parser;
mod tokenizer;

pub use parser::*;

use std::ops::{AddAssign, MulAssign};

use prost_reflect::ReflectMessage;

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

pub fn parse_text<M: ReflectMessage + Default>(text: &str) -> Result<M> {
    let empty_message = M::default();
    let descriptor = empty_message.descriptor();
    dbg!(descriptor);

    Err(ProtoError::Todo(String::new()))
    // todo!()
}

pub struct Deserializer<'de> {
    input: &'de str,
}

impl<'de> Deserializer<'de> {
    pub fn from_str(input: &'de str) -> Self {
        Deserializer { input }
    }
}

impl<'de> Deserializer<'de> {
    fn peek_char(&mut self) -> Result<char> {
        self.input
            .chars()
            .next()
            .ok_or(ProtoError::Todo(String::new()))
    }

    fn next_char(&mut self) -> Result<char> {
        let ch = self.peek_char()?;
        self.input = &self.input[ch.len_utf8()..];
        Ok(ch)
    }

    fn next_token(&self) -> Result<tokenizer::Token> {
        if self.try_consume_whitespace() {
            Ok(todo!())
        } else {
            todo!()
        }
    }

    fn try_consume_whitespace(&self) -> bool {
        let n = self.input.chars().take_while(|c| c == &' ').count();
        n > 0
    }

    fn parse_bool(&mut self) -> Result<bool> {
        let tvalues = ["true", "True", "t"];
        let fvalues = ["false", "False"];
        for &tv in &tvalues {
            if self.input.starts_with(tv) {
                self.input = &self.input[tv.len()..];
                return Ok(true);
            }
        }

        for &fv in &fvalues {
            if self.input.starts_with(fv) {
                self.input = &self.input[fv.len()..];
                return Ok(false);
            }
        }

        if let Ok(n) = self.parse_unsigned::<u64>() {
            return Ok(n != 0);
        }

        Err(ProtoError::Todo(String::new()))
    }

    fn parse_unsigned<T>(&mut self) -> Result<T>
    where
        T: AddAssign<T> + MulAssign<T> + From<u8>,
    {
        // let mut int = match self.next_char()? {

        // }
        todo!()
    }
}
