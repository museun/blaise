use crate::prelude::*;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub(crate) kind: ErrorKind,
    pub(crate) span: Span,
    pub(crate) source: String,
    pub(crate) file: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = self.source.lines().nth(self.span.row() - 1).unwrap();
        let (data, adjusted) = midpoint(line, self.span.column() - 1, 80);
        writeln!(f, "{}", data)?;
        writeln!(f, "{}", draw_caret(adjusted));
        write!(f, "{}:{} {}", self.file, self.span, self.kind)
    }
}

impl Error {
    pub fn new(
        kind: impl Into<ErrorKind>,
        span: Span,
        src: impl Into<String>,
        file: impl Into<String>,
    ) -> Self {
        Self {
            kind: kind.into(),
            span,
            source: src.into(),
            file: file.into(),
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
    Expected(Vec<TokenType>, TokenType),
    Unexpected(TokenType),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::Unknown(reason) => write!(f, "error message: {}", reason),
            ErrorKind::Expected(wanted, got) => {
                if wanted.len() > 1 {
                    write!(
                        f,
                        "expected one of: {}, got {}",
                        wanted
                            .iter()
                            .map(|s| format!("{}", s))
                            .fold(String::new(), |mut a, c| {
                                if !a.is_empty() {
                                    a.push_str(", ");
                                }
                                a.push_str(&c);
                                a
                            }),
                        got
                    )
                } else {
                    write!(f, "expected {} got {}", wanted[0], got)
                }
            }
            ErrorKind::Unexpected(token) => write!(f, "unexpected: {}", token),
        }
    }
}

impl<'a> From<&'a str> for ErrorKind {
    fn from(s: &'a str) -> Self {
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
    use std::iter::repeat;
    format!("{}^", repeat(" ").take(width).collect::<String>())
}
