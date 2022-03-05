use super::{ProtoError, Result};
use std::{cell::RefCell, io::Read, rc::Rc};

trait CharacterClass {
    fn is_in_class(c: u8) -> bool;
}

macro_rules! character_class {
    ($name:ident, $expr:expr) => {
        struct $name;
        impl CharacterClass for $name {
            fn is_in_class(c: u8) -> bool {
                $expr(c)
            }
        }
    };
}

character_class!(Whitespace, |c: u8| c.is_ascii_whitespace());
character_class!(WhitespaceNoNewline, |c| b" \t\t\r".contains(&c));
character_class!(Unprintable, |c| c < b' ' && c > 0);
character_class!(Digit, |c: u8| c.is_ascii_digit());
character_class!(OctalDigit, |c: u8| (b'0'..=b'7').contains(&c));
character_class!(HexDegit, |c: u8| c.is_ascii_hexdigit());
character_class!(Letter, |c: u8| c.is_ascii_alphabetic() || c == b'_');
character_class!(Alphanumeric, |c: u8| c.is_ascii_alphanumeric() || c == b'_');
character_class!(Escape, |c| b"abfnrtv\\?'\"".contains(&c));

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub type_: TokenType,
    pub text: Vec<u8>,
    pub line: usize,
    pub column: usize,
    pub end_column: usize,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            type_: TokenType::Start,
            text: Vec::new(),
            line: 0,
            column: 0,
            end_column: 0,
        }
    }
}

impl Token {
    pub fn str(&self) -> Result<&str> {
        std::str::from_utf8(&self.text)
            .map_err(|_| ProtoError::Todo("Failed to interpret as utf8".into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Start,
    Float,
    Integer,
    String,
    Ident,
    Symbol,
    Whitespace,
    NewLine,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentStyle {
    CppCommentStyle,
    ShellCommentStyle,
}

enum NextCommentStatus {
    LineComment,
    BlockComment,
    SlashNotComment,
    NoComment,
}

pub trait ErrorCollector: std::fmt::Debug {
    fn add_error(&mut self, line: usize, column: usize, message: &str) {}
    fn add_warning(&mut self, line: usize, column: usize, message: &str) {}
}

impl ErrorCollector for () {}

#[derive(Debug)]
pub struct Tokenizer<R> {
    reader: R,
    error_collector: Rc<RefCell<dyn ErrorCollector>>,
    buffer: Vec<u8>,
    buffer_size: usize,
    buffer_pos: usize,
    current: Token,
    previous: Token,
    current_char: u8,
    read_error: bool,
    line: usize,
    column: usize,
    record_target: Vec<u8>,
    record_start: Option<usize>,
    allow_f_after_float: bool,
    comment_style: CommentStyle,
    require_space_after_number: bool,
    allow_multiline_strings: bool,
    report_whitespace: bool,
    report_newlines: bool,
    tab_width: usize,
}

impl<R: Read> Tokenizer<R> {
    pub fn new(reader: R, error_collector: Rc<RefCell<dyn ErrorCollector>>) -> Self {
        let default_buffer_size = 8192;
        let mut this = Self {
            reader,
            error_collector: error_collector,
            buffer: vec![0; default_buffer_size],
            buffer_size: 0,
            buffer_pos: 0,
            current: Token::default(),
            previous: Token::default(),
            current_char: 0,
            read_error: false,
            line: 0,
            column: 0,
            record_target: Vec::new(),
            record_start: None,
            allow_f_after_float: false,
            comment_style: CommentStyle::CppCommentStyle,
            require_space_after_number: false,
            allow_multiline_strings: false,
            report_newlines: false,
            report_whitespace: false,
            tab_width: 8,
        };

        this.refresh();

        this
    }

    fn add_error(&mut self, message: &str) {
        self.error_collector
            .borrow_mut()
            .add_error(self.line, self.column, message);
    }

    pub fn set_report_whitespaces(&mut self, report: bool) {
        self.report_whitespace = report;
        self.report_newlines &= report;
    }

    pub fn set_report_newlines(&mut self, report: bool) {
        self.report_newlines = report;
        self.report_newlines |= report;
    }

    pub fn set_allow_f_after_float(&mut self, allow: bool) {
        self.allow_f_after_float = allow;
    }

    pub fn set_comment_style(&mut self, comment_style: CommentStyle) {
        self.comment_style = comment_style;
    }

    pub fn set_require_space_after_number(&mut self, require: bool) {
        self.require_space_after_number = require;
    }

    pub fn set_allow_multiline_strings(&mut self, allow: bool) {
        self.allow_multiline_strings = allow;
    }

    fn next_char(&mut self) {
        if self.current_char == b'\n' {
            self.line += 1;
            self.column = 0;
        } else if self.current_char == b'\t' {
            self.column += self.tab_width - self.column % self.tab_width;
        } else {
            self.column += 1;
        }

        self.buffer_pos += 1;
        if self.buffer_pos < self.buffer_size {
            self.current_char = self.buffer[self.buffer_pos];
        } else {
            self.refresh();
        }
    }

    fn refresh(&mut self) {
        if self.read_error {
            self.current_char = 0;
            return;
        }

        if let Some(start) = self.record_start {
            if start < self.buffer_size {
                self.record_target
                    .extend_from_slice(&self.buffer[start..self.buffer_size]);
                self.record_start = Some(0);
            }
        }

        self.buffer_pos = 0;
        loop {
            match self.reader.read(&mut self.buffer[..]) {
                Ok(n) => {
                    self.buffer_size = n;
                    if n == 0 {
                        self.read_error = true;
                        self.current_char = 0;
                        return;
                    }
                }
                Err(_) => {
                    self.read_error = true;
                    self.current_char = 0;
                    return;
                }
            }

            if self.buffer_size > 0 {
                break;
            }
        }

        self.current_char = self.buffer[0];
    }

    fn start_recording(&mut self) {
        self.record_start = Some(self.buffer_pos);
    }

    fn stop_recording(&mut self) -> Vec<u8> {
        if Some(self.buffer_pos) != self.record_start {
            self.record_target
                .extend_from_slice(&self.buffer[self.record_start.unwrap()..self.buffer_pos]);
        }
        let ret = self.record_target.clone();
        self.record_target.clear();
        self.record_start = None;

        return ret;
    }

    fn start_token(&mut self) {
        self.current.type_ = TokenType::Start;
        self.current.column = self.column;
        self.current.line = self.line;
        self.start_recording()
    }

    fn end_token(&mut self) {
        self.current.text = self.stop_recording();
        self.current.end_column = self.column;
    }

    fn looking_at<C: CharacterClass>(&self) -> bool {
        C::is_in_class(self.current_char)
    }

    fn try_consume_one<C: CharacterClass>(&mut self) -> bool {
        if self.looking_at::<C>() {
            self.next_char();
            true
        } else {
            false
        }
    }

    fn try_consume_some<C: CharacterClass>(&mut self, n: usize) -> bool {
        for _ in 0..n {
            if !self.try_consume_one::<C>() {
                return false;
            }
        }
        true
    }

    fn try_consume(&mut self, c: u8) -> bool {
        if self.current_char == c {
            self.next_char();
            true
        } else {
            false
        }
    }

    fn consume_zero_or_more<C: CharacterClass>(&mut self) {
        while C::is_in_class(self.current_char) {
            self.next_char();
        }
    }

    fn consume_one_or_more<C: CharacterClass>(&mut self, error: &str) {
        if !C::is_in_class(self.current_char) {
            self.add_error(error.into());
        } else {
            loop {
                self.next_char();
                if !C::is_in_class(self.current_char) {
                    break;
                }
            }
        }
    }

    fn consume_string(&mut self, delimeter: u8) {
        loop {
            match self.current_char {
                0 => {
                    self.add_error("Unexpected EoS".into());
                    return;
                }
                b'\n' => {
                    if !self.allow_multiline_strings {
                        self.add_error("String literals cannot cross line boundaries.".into());
                        return;
                    }
                    self.next_char();
                }
                b'\\' => {
                    // an escape sequence
                    self.next_char();
                    if self.try_consume_one::<Escape>() {
                        // valid escape sequence
                    } else if self.try_consume_one::<OctalDigit>() {
                        // Possibly followed by two more octal digits, but these will
                        // just be consumed by the main loop anyway so we don't need
                        // to do so explicitly here.
                    } else if self.try_consume(b'x') {
                        if !self.try_consume_one::<HexDegit>() {
                            self.add_error("Expected hex digits for escape sequence.".into());
                        }
                        // Possibly followed by another hex digit, but again we don't care.
                    } else if self.try_consume(b'u') {
                        if !self.try_consume_some::<HexDegit>(4) {
                            self.add_error("Expected hex digits for \\u excape sequence.".into());
                        }
                    } else if self.try_consume(b'U') {
                        if !self.try_consume(b'0')
                            || !self.try_consume(b'0')
                            || !(self.try_consume(b'1') || !self.try_consume(b'0'))
                            || !self.try_consume_some::<HexDegit>(5)
                        {
                            self.add_error(
                                "Expected eight hex digits up to 10ffff for \\U escape sequence."
                                    .into(),
                            );
                        }
                    } else {
                        self.add_error("Invalid escape sequence in string literal.".into());
                    }
                }
                _ => {
                    if self.current_char == delimeter {
                        self.next_char();
                        return;
                    }
                    self.next_char();
                }
            }
        }
    }

    fn consume_number(&mut self, started_with_zero: bool, started_with_dot: bool) -> TokenType {
        let mut is_float = false;
        if started_with_zero && (self.try_consume(b'x') || self.try_consume(b'X')) {
            // A hex number
            self.consume_one_or_more::<HexDegit>("'0x' must be followed by hex digits.");
        } else if started_with_zero && self.looking_at::<Digit>() {
            // a octal number
            self.consume_zero_or_more::<OctalDigit>();
            if self.looking_at::<Digit>() {
                self.add_error("Numbers starting with leading zero must be in octal.");
                self.consume_zero_or_more::<Digit>();
            }
        } else {
            // a decimal number
            if started_with_dot {
                is_float = true;
                self.consume_zero_or_more::<Digit>();
            } else {
                self.consume_zero_or_more::<Digit>();

                if self.try_consume(b'.') {
                    is_float = true;
                    self.consume_zero_or_more::<Digit>();
                }
            }

            if self.try_consume(b'e') || self.try_consume(b'E') {
                is_float = true;
                let _ = self.try_consume(b'-') || self.try_consume(b'+');
                self.consume_one_or_more::<Digit>("'e' must be followed by exponent.");
            }

            if self.allow_f_after_float && (self.try_consume(b'f') || self.try_consume(b'F')) {
                is_float = true;
            }
        }

        if self.looking_at::<Letter>() && self.require_space_after_number {
            self.add_error("Need space between number and identifier.");
        } else if self.current_char == b'.' {
            if is_float {
                self.add_error("Already saw decimal point or exponent; can't have another one.");
            } else {
                self.add_error("Hex and octal numbers must be integers.");
            }
        }

        if is_float {
            TokenType::Float
        } else {
            TokenType::Integer
        }
    }

    fn consume_line_comment(&mut self) -> Vec<u8> {
        self.start_recording();

        while self.current_char != 0 && self.current_char != b'\n' {
            self.next_char();
        }
        self.try_consume(b'\n');

        self.stop_recording()
    }

    fn consume_block_comment(&mut self) -> Vec<u8> {
        let start_line = self.line;
        let start_column = self.column - 2;

        self.start_recording();

        loop {
            while !b"\0*/\n".contains(&self.current_char) {
                self.next_char();
            }

            if self.try_consume(b'\n') {
                let record = self.stop_recording();

                self.consume_zero_or_more::<WhitespaceNoNewline>();

                if self.try_consume(b'*') && self.try_consume(b'/') {
                    break record;
                }

                self.record_target = record;
                self.start_recording();
            } else if self.try_consume(b'*') && self.try_consume(b'/') {
                let mut record = self.stop_recording();
                // strip trailing '*/'
                record.pop();
                record.pop();
                break record;
            } else if self.try_consume(b'/') && self.current_char == b'*' {
                self.add_error("'/*' inside block comment. Block comments cannot be nested.");
                break self.stop_recording();
            } else if self.current_char == 0 {
                self.add_error("End-of-file inside block comment.");
                self.error_collector.borrow_mut().add_error(
                    start_line,
                    start_column,
                    " Comment started here.",
                );
                break self.stop_recording();
            }
        }
    }

    fn try_consume_comment_start(&mut self) -> NextCommentStatus {
        if self.comment_style == CommentStyle::CppCommentStyle && self.try_consume(b'/') {
            if self.try_consume(b'/') {
                return NextCommentStatus::LineComment;
            } else if self.try_consume(b'*') {
                return NextCommentStatus::BlockComment;
            } else {
                self.current.type_ = TokenType::Symbol;
                self.current.text = b"/".to_vec();
                self.current.line = self.line;
                self.current.column = self.column - 1;
                self.current.end_column = self.column;
                return NextCommentStatus::SlashNotComment;
            }
        } else if self.comment_style == CommentStyle::ShellCommentStyle && self.try_consume(b'#') {
            return NextCommentStatus::LineComment;
        } else {
            return NextCommentStatus::NoComment;
        }
    }

    fn try_consume_whitespace(&mut self) -> bool {
        if self.report_newlines {
            if self.try_consume_one::<WhitespaceNoNewline>() {
                self.consume_zero_or_more::<WhitespaceNoNewline>();
                self.current.type_ = TokenType::Whitespace;
                return true;
            }
            return false;
        }

        if self.try_consume_one::<Whitespace>() {
            self.consume_zero_or_more::<Whitespace>();
            self.current.type_ = TokenType::Whitespace;
            return self.report_whitespace;
        }
        false
    }

    fn try_consume_newline(&mut self) -> bool {
        if !self.report_whitespace || !self.report_newlines {
            return false;
        }

        if self.try_consume(b'\n') {
            self.current.type_ = TokenType::NewLine;
            return true;
        }
        return false;
    }

    pub fn next(&mut self) -> bool {
        self.previous = self.current.clone();
        while !self.read_error {
            self.start_token();
            let report_token = self.try_consume_whitespace() || self.try_consume_newline();
            self.end_token();
            if report_token {
                return true;
            }

            match self.try_consume_comment_start() {
                NextCommentStatus::LineComment => {
                    self.consume_line_comment();
                    continue;
                }
                NextCommentStatus::BlockComment => {
                    self.consume_block_comment();
                    continue;
                }
                NextCommentStatus::SlashNotComment => {
                    return true;
                }
                NextCommentStatus::NoComment => {}
            };

            if self.read_error {
                break;
            }

            if self.looking_at::<Unprintable>() || self.current_char == 0 {
                self.add_error("Invalid control characters encounterd in text.");
                self.next_char();
                while self.try_consume_one::<Unprintable>()
                    || (!self.read_error && self.try_consume(0))
                {
                    // ignore
                }
            } else {
                self.start_token();

                let type_ = if self.try_consume_one::<Letter>() {
                    self.consume_zero_or_more::<Alphanumeric>();
                    TokenType::Ident
                } else if self.try_consume(b'0') {
                    self.consume_number(true, false)
                } else if self.try_consume(b'.') {
                    if self.try_consume_one::<Digit>() {
                        if self.previous.type_ == TokenType::Ident
                            && self.current.line == self.previous.line
                            && self.current.column == self.current.end_column
                        {
                            self.error_collector.borrow_mut().add_error(
                                self.line,
                                self.column - 2,
                                "Need space between identifier and decimal point.".into(),
                            )
                        }
                        self.consume_number(false, true)
                    } else {
                        TokenType::Symbol
                    }
                } else if self.try_consume_one::<Digit>() {
                    self.consume_number(false, false)
                } else if self.try_consume(b'"') {
                    self.consume_string(b'"');
                    TokenType::String
                } else if self.try_consume(b'\'') {
                    self.consume_string(b'\'');
                    TokenType::String
                } else {
                    if self.current_char & 0x80 > 0 {
                        self.add_error(&format!(
                            "Interpreting non ascii codepoint: {}",
                            self.current_char
                        ));
                    }
                    self.next_char();
                    TokenType::Symbol
                };
                self.current.type_ = type_;

                self.end_token();
                return true;
            }
        }

        self.current.type_ = TokenType::Eof;
        self.current.text.clear();
        self.current.line = self.line;
        self.current.column = self.column;
        self.current.end_column = self.column;
        return false;
    }

    pub fn current(&self) -> &Token {
        &self.current
    }

    pub fn previous(&self) -> &Token {
        &self.previous
    }
}

fn digit_value(c: u8) -> u32 {
    if c.is_ascii_digit() {
        return (c - b'0') as u32;
    }
    if c.is_ascii_lowercase() {
        return (c - b'a' + 10) as u32;
    }
    if c.is_ascii_uppercase() {
        return (c - b'A' + 10) as u32;
    }
    panic!()
}
pub fn parse_string_append(text: &[u8], output: &mut String) {
    // TODO: support utf8 input?
    if text.len() == 0 {
        panic!("parse_string_append() passed text tha could not have benn tokenized as a string");
    }

    let mut idx = 1;
    while idx < text.len() {
        if text[idx] == b'\\' && idx + 1 < text.len() {
            idx += 1;
            if OctalDigit::is_in_class(text[idx]) {
                let mut code = digit_value(text[idx]);
                if idx + 1 < text.len() && OctalDigit::is_in_class(text[idx + 1]) {
                    idx += 1;
                    code = code * 8 + digit_value(text[idx]);
                }
                if idx + 1 < text.len() && OctalDigit::is_in_class(text[idx + 1]) {
                    idx += 1;
                    code = code * 8 + digit_value(text[idx]);
                }

                output.push(code as u8 as char);
            } else if text[idx] == b'x' {
                let mut code = 0;
                if idx + 1 < text.len() && HexDegit::is_in_class(text[idx + 1]) {
                    idx += 1;
                    code = digit_value(text[idx]);
                }
                if idx + 1 < text.len() && HexDegit::is_in_class(text[idx + 1]) {
                    idx += 1;
                    code = code * 16 + digit_value(text[idx]);
                }
                output.push(code as u8 as char);
            } else if text[idx] == b'u' || text[idx] == b'U' {
                let (unicode, len) = fetch_unicode_point(&text[idx..]);
                if len == 0 {
                    output.push(text[idx] as char);
                } else {
                    append_utf8(unicode, output);
                    idx += len - 1;
                }
            } else {
                output.push(translate_escape(text[idx]));
            }
        } else if text[idx] == text[0] && idx == text.len() - 1 {
            // ignore final quote matching the starting quote.
        } else {
            output.push(text[idx] as char);
        }
        idx += 1;
    }
}

fn translate_escape(c: u8) -> char {
    match c {
        b'a' => '\x07',
        b'b' => '\x08',
        b'f' => '\x0C',
        b'n' => '\n',
        b'r' => '\r',
        b't' => '\t',
        b'v' => '\x0b',
        b'\\' => '\\',
        b'?' => '?',
        b'\'' => '\'',
        b'"' => '"',
        _ => '?',
    }
}

fn unicode_length(key: u8) -> usize {
    if key == b'u' {
        4
    } else if key == b'U' {
        8
    } else {
        panic!()
    }
}

fn read_hex_digits(text: &[u8], len: usize) -> Option<u32> {
    let mut result = 0;
    if len == 0 {
        return None;
    }

    if text.len() < len {
        return None;
    }

    for c in text {
        result = (result << 4) + digit_value(*c);
    }
    Some(result)
}

const MIN_HEAD_SURROGATE: u32 = 0xd800;
const MAX_HEAD_SURROGATE: u32 = 0xdc00;
const MIN_TRAIL_SURROGATE: u32 = 0xdc00;
const MAX_TRAIL_SURROGATE: u32 = 0xe000;

fn is_head_surrogate(code_point: u32) -> bool {
    (MIN_HEAD_SURROGATE..MAX_HEAD_SURROGATE).contains(&code_point)
}

fn is_trail_surrogate(code_point: u32) -> bool {
    (MIN_TRAIL_SURROGATE..MAX_TRAIL_SURROGATE).contains(&code_point)
}

fn assemble_utf16(head_surrogate: u32, trail_surrogate: u32) -> u32 {
    assert!(is_head_surrogate(head_surrogate));
    assert!(is_trail_surrogate(trail_surrogate));
    0x10000
        + (((head_surrogate - MIN_HEAD_SURROGATE) << 10) | trail_surrogate - MIN_TRAIL_SURROGATE)
}

/// Returns unicode code point and length of escaped sequence of text
fn fetch_unicode_point(text: &[u8]) -> (u32, usize) {
    let len = unicode_length(text[0]);
    let mut code_point = if let Some(code_point) = read_hex_digits(&text[1..], len) {
        code_point
    } else {
        return (0, 0);
    };
    let mut idx = 1;
    idx += len;

    if is_head_surrogate(code_point) && text[idx] == b'\\' && text[idx + 1] == b'u' {
        if let Some(tail_surrogate) = read_hex_digits(&text[idx + 2..], 4) {
            if is_trail_surrogate(tail_surrogate) {
                code_point = assemble_utf16(code_point, tail_surrogate);
                idx += 6;
            }
        }
    }

    (code_point, idx)
}

fn append_utf8(code_point: u32, output: &mut String) {
    if let Some(c) = std::char::from_u32(code_point) {
        output.push(c);
    } else {
        output.push_str(&format!("\\U{:08x}", code_point));
    }
}

pub fn parse_integer(text: &[u8], max_value: u64) -> Result<u64> {
    let mut base = 10;
    let mut idx = 0;
    if text[0] == b'0' {
        if text.len() >= 2 && (text[1] == b'x' || text[1] == b'X') {
            base = 16;
            idx = 2;
        } else {
            base = 8;
        }
    }
    let s = std::str::from_utf8(&text[idx..]).map_err(|_| ProtoError::Todo("".into()))?;

    let val = u64::from_str_radix(s, base).map_err(|_| ProtoError::Todo("".into()))?;

    if val <= max_value {
        Ok(val)
    } else {
        Err(ProtoError::Todo("".into()))
    }
}

// #[derive(Clone)]
// enum TokenValue {
//     Start,
//     EOF,
//     Ident(String),
//     Unsigned(u64),
//     Signed(i64),
//     NewLine,
//     Whitespace,
//     Slash,
// }

#[cfg(test)]
mod tests {
    use super::*;

    struct TestReader {
        data: Vec<u8>,
        block_size: usize,
        pos: usize,
    }

    impl TestReader {
        fn new(data: Vec<u8>, block_size: usize) -> Self {
            Self {
                data,
                block_size,
                pos: 0,
            }
        }
    }

    impl Read for TestReader {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            if self.pos == self.data.len() {
                return Ok(0);
            }
            let n = buf
                .len()
                .min(self.block_size)
                .min(self.data.len() - self.pos);
            buf[..n].copy_from_slice(&self.data[self.pos..self.pos + n]);
            self.pos += n;

            Ok(n)
        }
    }

    #[derive(Debug, Default)]
    struct TestErrorCollecter {
        errors: Vec<String>,
    }

    impl ErrorCollector for TestErrorCollecter {
        fn add_error(&mut self, line: usize, column: usize, message: &str) {
            self.errors.push(message.to_string());
        }
    }

    type Tokenizer = super::Tokenizer<TestReader>;

    #[rstest::rstest(
        token_case => [
            (b"hello".to_vec(), TokenType::Ident),
            (b"123".to_vec(), TokenType::Integer),
            (b"0xab6".to_vec(), TokenType::Integer),
            (b"0XAB6".to_vec(), TokenType::Integer),
            (b"0X1234567".to_vec(), TokenType::Integer),
            (b"0x89abcdef".to_vec(), TokenType::Integer),
            (b"0x90ABCDEF".to_vec(), TokenType::Integer),
            (b"01234567".to_vec(), TokenType::Integer),
            (b"123.45".to_vec(), TokenType::Float),
            (b"1.".to_vec(), TokenType::Float),
            (b"1e3".to_vec(), TokenType::Float),
            (b"1E3".to_vec(), TokenType::Float),
            (b"1e-3".to_vec(), TokenType::Float),
            (b"1.e3".to_vec(), TokenType::Float),
            (b"1.2e3".to_vec(), TokenType::Float),
            (b".1".to_vec(), TokenType::Float),
            (b".1e3".to_vec(), TokenType::Float),
            (b".1e-3".to_vec(), TokenType::Float),
            (b".1e+3".to_vec(), TokenType::Float),
            (b"'hello'".to_vec(), TokenType::String),
            (br#""foo""#.to_vec(), TokenType::String),
            (br#"'a"b'"#.to_vec(), TokenType::String),
            (br#""a'b""#.to_vec(), TokenType::String),
            (br#"'a\'b'"#.to_vec(), TokenType::String),
            (br#""a\"b""#.to_vec(), TokenType::String),
            (br#"'\xf'"#.to_vec(), TokenType::String),
            (br#"'\0'"#.to_vec(), TokenType::String),
            (b"+".to_vec(), TokenType::Symbol),
            (b".".to_vec(), TokenType::Symbol),
            (b"/".to_vec(), TokenType::Symbol),
        ],
        block_size => [
            1, 2, 3, 5, 7, 13, 32, 1024
        ]
    )]
    fn test_simple_token(token_case: (Vec<u8>, TokenType), block_size: usize) {
        let reader = TestReader::new(token_case.0.clone(), block_size);
        let error_collector = Rc::new(RefCell::new(TestErrorCollecter::default()));
        let mut tokenizer = Tokenizer::new(
            reader,
            Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>,
        );
        assert_eq!(TokenType::Start, tokenizer.current().type_);
        assert_eq!(b"", &tokenizer.current().text[..]);
        assert_eq!(0, tokenizer.current().line);
        assert_eq!(0, tokenizer.current().column);
        assert_eq!(0, tokenizer.current().end_column);

        assert!(tokenizer.next());
        assert_eq!(token_case.1, tokenizer.current().type_);
        assert_eq!(token_case.0, tokenizer.current().text);
        assert_eq!(0, tokenizer.current().line);
        assert_eq!(0, tokenizer.current().column);
        assert_eq!(token_case.0.len(), tokenizer.current().end_column);

        assert!(!tokenizer.next());
        assert_eq!(TokenType::Eof, tokenizer.current().type_);
        assert_eq!(b"", &tokenizer.current().text[..]);
        assert_eq!(0, tokenizer.current().line);
        assert_eq!(token_case.0.len(), tokenizer.current().column);
        assert_eq!(token_case.0.len(), tokenizer.current().end_column);

        assert!(error_collector.borrow().errors.is_empty());
    }

    #[rstest::rstest]
    fn test_float_suffix(#[values(1, 2, 3, 5, 7, 13, 32, 1024)] block_size: usize) {
        let text = b"1f 2.5f 6e3f 7F".to_vec();
        let reader = TestReader::new(text, block_size);
        let error_collector = Rc::new(RefCell::new(TestErrorCollecter::default()));
        let mut tokenizer = Tokenizer::new(
            reader,
            Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>,
        );
        tokenizer.set_allow_f_after_float(true);

        assert!(tokenizer.next());
        assert_eq!(b"1f", &tokenizer.current().text[..]);
        assert_eq!(TokenType::Float, tokenizer.current().type_);
        assert!(tokenizer.next());
        assert_eq!(b"2.5f", &tokenizer.current().text[..]);
        assert_eq!(TokenType::Float, tokenizer.current().type_);
        assert!(tokenizer.next());
        assert_eq!(b"6e3f", &tokenizer.current().text[..]);
        assert_eq!(TokenType::Float, tokenizer.current().type_);
        assert!(tokenizer.next());
        assert_eq!(b"7F", &tokenizer.current().text[..]);
        assert_eq!(TokenType::Float, tokenizer.current().type_);

        assert!(!tokenizer.next());
        assert!(error_collector.borrow().errors.is_empty());
    }

    #[rstest::rstest(
        token_case => [
            (b" ".to_vec(), TokenType::Whitespace),
            (b"    ".to_vec(), TokenType::Whitespace),
            (b"\t".to_vec(), TokenType::Whitespace),
            (b"\t ".to_vec(), TokenType::Whitespace),
            (b" \t".to_vec(), TokenType::Whitespace),
            (b"  \t\r".to_vec(), TokenType::Whitespace),
            (b"\n".to_vec(), TokenType::NewLine),
        ],
        block_size => [1, 2, 3, 5, 7, 13, 32, 1024]
      )]
    fn test_whitespace(token_case: (Vec<u8>, TokenType), block_size: usize) {
        {
            let reader = TestReader::new(token_case.0.clone(), block_size);
            let error_collector = Rc::new(RefCell::new(TestErrorCollecter::default()));
            let mut tokenizer = Tokenizer::new(
                reader,
                Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>,
            );

            assert!(!tokenizer.next())
        }
        {
            let reader = TestReader::new(token_case.0.clone(), block_size);
            let error_collector = Rc::new(RefCell::new(TestErrorCollecter::default()));
            let mut tokenizer = Tokenizer::new(
                reader,
                Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>,
            );
            tokenizer.set_report_newlines(true);
            tokenizer.set_report_whitespaces(true);

            assert!(tokenizer.next());
            assert_eq!(token_case.0, tokenizer.current().text);
            assert_eq!(token_case.1, tokenizer.current().type_);

            assert!(!tokenizer.next());
        }
    }

    #[rstest::rstest(
        case => [
            (&b""[..], vec![Token { type_: TokenType::Eof, text: b"".to_vec(), line: 0, column: 0, end_column: 0 }]),
            (&b"foo 1 1.2 + 'bar'"[..], vec![
                Token { type_: TokenType::Ident, text: b"foo".to_vec(), line: 0, column: 0, end_column: 3 },
                Token { type_: TokenType::Integer, text: b"1".to_vec(), line: 0, column: 4, end_column: 5 },
                Token { type_: TokenType::Float, text: b"1.2".to_vec(), line: 0, column: 6, end_column: 9 },
                Token { type_: TokenType::Symbol, text: b"+".to_vec(), line: 0, column: 10, end_column: 11 },
                Token { type_: TokenType::String, text: b"'bar'".to_vec(), line: 0, column: 12, end_column: 17 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 0, column: 17, end_column: 17 },
            ]),
            (&b"!@+%"[..], vec![
                Token { type_: TokenType::Symbol, text: b"!".to_vec(), line: 0, column: 0, end_column: 1 },
                Token { type_: TokenType::Symbol, text: b"@".to_vec(), line: 0, column: 1, end_column: 2 },
                Token { type_: TokenType::Symbol, text: b"+".to_vec(), line: 0, column: 2, end_column: 3 },
                Token { type_: TokenType::Symbol, text: b"%".to_vec(), line: 0, column: 3, end_column: 4 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 0, column: 4, end_column: 4 },

            ]),
            (&b"foo bar\nrab oof"[..], vec![
                Token { type_: TokenType::Ident, text: b"foo".to_vec(), line: 0, column: 0, end_column: 3 },
                Token { type_: TokenType::Ident, text: b"bar".to_vec(), line: 0, column: 4, end_column: 7 },
                Token { type_: TokenType::Ident, text: b"rab".to_vec(), line: 1, column: 0, end_column: 3 },
                Token { type_: TokenType::Ident, text: b"oof".to_vec(), line: 1, column: 4, end_column: 7 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 1, column: 7, end_column: 7 },
            ]),
            (&b"\"foo\tbar\" baz"[..], vec![
                Token { type_: TokenType::String, text: b"\"foo\tbar\"".to_vec(), line: 0, column: 0, end_column: 12 },
                Token { type_: TokenType::Ident, text: b"baz".to_vec(), line: 0, column: 13, end_column: 16 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 0, column: 16, end_column: 16 },
            ]),
            (&b"foo // This is a comment\nbar // This is another commend"[..], vec![
                Token { type_: TokenType::Ident, text: b"foo".to_vec(), line: 0, column: 0, end_column: 3 },
                Token { type_: TokenType::Ident, text: b"bar".to_vec(), line: 1, column: 0, end_column: 3 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 1, column: 30, end_column: 30 },
            ]),
            (&b"foo /* This is a block comment */ bar"[..], vec![
                Token { type_: TokenType::Ident, text: b"foo".to_vec(), line: 0, column: 0, end_column: 3 },
                Token { type_: TokenType::Ident, text: b"bar".to_vec(), line: 0, column: 34, end_column: 37 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 0, column: 37, end_column: 37 },
            ]),
            (&b"foo # bar\nbaz"[..], vec![
                Token { type_: TokenType::Ident, text: b"foo".to_vec(), line: 0, column: 0, end_column: 3 },
                Token { type_: TokenType::Symbol, text: b"#".to_vec(), line: 0, column: 4, end_column: 5 },
                Token { type_: TokenType::Ident, text: b"bar".to_vec(), line: 0, column: 6, end_column: 9 },
                Token { type_: TokenType::Ident, text: b"baz".to_vec(), line: 1, column: 0, end_column: 3 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 1, column: 3, end_column: 3 },
            ]),
            (&b"foo\n\t\rbar"[..], vec![
                Token { type_: TokenType::Ident, text: b"foo".to_vec(), line: 0, column: 0, end_column: 3 },
                Token { type_: TokenType::Ident, text: b"bar".to_vec(), line: 1, column: 9, end_column: 12 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 1, column: 12, end_column: 12 },
            ]),
        ],
        block_size => [1, 2, 3, 5, 7, 13, 32, 1024],
    )]
    fn test_multi_token(case: (&[u8], Vec<Token>), block_size: usize) {
        let reader = TestReader::new(case.0.to_vec(), block_size);
        let error_collector = Rc::new(RefCell::new(TestErrorCollecter::default()));
        let mut tokenizer = Tokenizer::new(
            reader,
            Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>,
        );

        assert_eq!(
            &Token {
                type_: TokenType::Start,
                text: b"".to_vec(),
                line: 0,
                column: 0,
                end_column: 0,
            },
            tokenizer.current()
        );

        for expected_token in &case.1 {
            let previous = tokenizer.current().clone();

            if expected_token.type_ != TokenType::Eof {
                assert!(tokenizer.next());
            } else {
                assert!(!tokenizer.next());
            }

            assert_eq!(&previous, tokenizer.previous());
            assert_eq!(expected_token, tokenizer.current());
        }

        assert!(error_collector.borrow().errors.is_empty());
    }

    #[rstest::rstest(
        case => [
            (&b"foo 1 \t1.2  \n   +\r'bar'"[..], vec![
                Token { type_: TokenType::Ident, text: b"foo".to_vec(), line: 0, column: 0, end_column: 3 },
                Token { type_: TokenType::Whitespace, text: b" ".to_vec(), line: 0, column: 3, end_column: 4 },
                Token { type_: TokenType::Integer, text: b"1".to_vec(), line: 0, column: 4, end_column: 5 },
                Token { type_: TokenType::Whitespace, text: b" \t".to_vec(), line: 0, column: 5, end_column: 8 },
                Token { type_: TokenType::Float, text: b"1.2".to_vec(), line: 0, column: 8, end_column: 11 },
                Token { type_: TokenType::Whitespace, text: b"  ".to_vec(), line: 0, column: 11, end_column: 13 },
                Token { type_: TokenType::NewLine, text: b"\n".to_vec(), line: 0, column: 13, end_column: 0 },
                Token { type_: TokenType::Whitespace, text: b"   ".to_vec(), line: 1, column: 0, end_column: 3 },
                Token { type_: TokenType::Symbol, text: b"+".to_vec(), line: 1, column: 3, end_column: 4 },
                Token { type_: TokenType::Whitespace, text: b"\r".to_vec(), line: 1, column: 4, end_column: 5 },
                Token { type_: TokenType::String, text: b"'bar'".to_vec(), line: 1, column: 5, end_column: 10 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 1, column: 10, end_column: 10 },
            ])
        ],
        block_size => [1, 2, 3, 5, 7, 13, 32, 1024],
    )]
    fn test_multi_token_whitespace(case: (&[u8], Vec<Token>), block_size: usize) {
        let reader = TestReader::new(case.0.to_vec(), block_size);
        let error_collector = Rc::new(RefCell::new(TestErrorCollecter::default()));
        let mut tokenizer = Tokenizer::new(
            reader,
            Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>,
        );
        tokenizer.set_report_newlines(true);
        tokenizer.set_report_whitespaces(true);

        assert_eq!(
            &Token {
                type_: TokenType::Start,
                text: b"".to_vec(),
                line: 0,
                column: 0,
                end_column: 0,
            },
            tokenizer.current()
        );

        for expected_token in &case.1 {
            let previous = tokenizer.current().clone();

            if expected_token.type_ != TokenType::Eof {
                assert!(tokenizer.next());
            } else {
                assert!(!tokenizer.next());
            }

            assert_eq!(&previous, tokenizer.previous());
            assert_eq!(expected_token, tokenizer.current());
        }

        assert!(error_collector.borrow().errors.is_empty());
    }

    #[rstest::rstest(
        case => [
            (&b"foo # bar\n\
            baz // qux\n\
            corge /* grault */\n\
            garply"[..], vec![
                Token { type_: TokenType::Ident, text: b"foo".to_vec(), line: 0, column: 0, end_column: 3 },
                Token { type_: TokenType::Ident, text: b"baz".to_vec(), line: 1, column: 0, end_column: 3 },
                Token { type_: TokenType::Symbol, text: b"/".to_vec(), line: 1, column: 4, end_column: 5 },
                Token { type_: TokenType::Symbol, text: b"/".to_vec(), line: 1, column: 5, end_column: 6 },
                Token { type_: TokenType::Ident, text: b"qux".to_vec(), line: 1, column: 7, end_column: 10 },
                Token { type_: TokenType::Ident, text: b"corge".to_vec(), line: 2, column: 0, end_column: 5 },
                Token { type_: TokenType::Symbol, text: b"/".to_vec(), line: 2, column: 6, end_column: 7 },
                Token { type_: TokenType::Symbol, text: b"*".to_vec(), line: 2, column: 7, end_column: 8 },
                Token { type_: TokenType::Ident, text: b"grault".to_vec(), line: 2, column: 9, end_column: 15 },
                Token { type_: TokenType::Symbol, text: b"*".to_vec(), line: 2, column: 16, end_column: 17 },
                Token { type_: TokenType::Symbol, text: b"/".to_vec(), line: 2, column: 17, end_column: 18 },
                Token { type_: TokenType::Ident, text: b"garply".to_vec(), line: 3, column: 0, end_column: 6 },
                Token { type_: TokenType::Eof, text: b"".to_vec(), line: 3, column: 6, end_column: 6 },
            ])
        ],
        block_size => [1, 2, 3, 5, 7, 13, 32, 1024],
    )]
    fn test_shell_comment_style(case: (&[u8], Vec<Token>), block_size: usize) {
        let reader = TestReader::new(case.0.to_vec(), block_size);
        let error_collector = Rc::new(RefCell::new(TestErrorCollecter::default()));
        let mut tokenizer = Tokenizer::new(
            reader,
            Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>,
        );
        tokenizer.set_comment_style(CommentStyle::ShellCommentStyle);

        assert_eq!(
            &Token {
                type_: TokenType::Start,
                text: b"".to_vec(),
                line: 0,
                column: 0,
                end_column: 0,
            },
            tokenizer.current()
        );

        for expected_token in &case.1 {
            let previous = tokenizer.current().clone();

            if expected_token.type_ != TokenType::Eof {
                assert!(tokenizer.next());
            } else {
                assert!(!tokenizer.next());
            }

            assert_eq!(&previous, tokenizer.previous());
            assert_eq!(expected_token, tokenizer.current());
        }

        assert!(error_collector.borrow().errors.is_empty());
    }
}
