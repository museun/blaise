use crate::prelude::*;

use std::io::prelude::*;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub(crate) kind: ErrorKind,
    pub(crate) span: Span,
    pub(crate) source: String,
    pub(crate) file: String,
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

    pub fn print(&self, w: &mut Writer) {
        let line = self.source.lines().nth(self.span.row() - 1).unwrap();
        let (data, adjusted) = midpoint(line, self.span.column() - 1, 80);

        writeln!(w, "{}", data);
        w.wrap(Color::Red, draw_caret(adjusted));
        writeln!(w);

        write!(w, "{}:{} ", self.file, self.span);

        match self.kind() {
            ErrorKind::Unknown(reason) => {
                w.wrap(Color::Yellow, "error message: ");
                write!(w, "{}", reason);
            }
            ErrorKind::Expected(wanted, got) => {
                if wanted.len() > 1 {
                    w.wrap(Color::Yellow, "expected one of: ");
                    w.wrap(
                        Color::Green,
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
                    );
                } else {
                    w.wrap(Color::Yellow, "expected: ");
                    w.wrap(Color::Green, &format!("{}", wanted[0]));
                }
                write!(w, " got ");
                w.wrap(Color::Red, &format!("{}", got));
            }
            ErrorKind::Unexpected(token) => {
                w.wrap(Color::Yellow, "unexpected: ");
                w.wrap(Color::Red, &format!("{}", token));
            }
        }
        writeln!(w);
        w.flush().unwrap();
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Unknown(String),
    Expected(Vec<TokenType>, TokenType),
    Unexpected(TokenType),
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
