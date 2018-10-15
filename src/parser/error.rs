use crate::prelude::*;
use std::fmt;

#[derive(PartialEq)]
pub struct Error {
    pub(crate) kind: ErrorKind,
    pub(crate) span: Span,
    pub(crate) source: String,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = self.source.lines().nth(self.span.row() - 1).unwrap();
        let (data, adjusted) = midpoint(line, self.span.column() - 1, 80);
        writeln!(f, "{}", data)?;
        writeln!(f, "{}", draw_caret(adjusted));
        write!(f, "{} {:?}", self.span, self.kind)
    }
}

impl Error {
    pub fn new(kind: impl Into<ErrorKind>, span: Span, src: impl Into<String>) -> Self {
        Self {
            kind: kind.into(),
            span,
            source: src.into(),
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Unknown(String),
    Expected(Token),
    Unexpected(Token),
}

impl From<Token> for ErrorKind {
    fn from(token: Token) -> Self {
        ErrorKind::Expected(token)
    }
}

impl<'a> From<&'a str> for ErrorKind {
    fn from(s: &'a str) -> Self {
        if let Some(t) = Token::try_parse(s) {
            return t.into();
        }
        ErrorKind::Unknown(s.into())
    }
}

fn midpoint(input: &str, cursor: usize, width: usize) -> (&str, usize) {
    let half = width / 2;
    if input.len() > width {
        if cursor < half {
            (&input[..half], cursor)
        } else {
            (&input[cursor - half..], half)
        }
    } else {
        (input, cursor)
    }
}

fn draw_caret(width: usize) -> String {
    let s = ::std::iter::repeat(" ").take(width).collect::<String>();
    format!("{}^", s)
}
