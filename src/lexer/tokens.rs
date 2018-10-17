use super::span::Span;
use super::token::{Token, TokenType};

use std::fmt;
use std::io::prelude::*;

#[derive(Clone)]
pub struct Tokens {
    data: Vec<Token>,
    pos: usize,
}

impl Tokens {
    pub(crate) fn new(data: Vec<Token>) -> Self {
        Self { data, pos: 0 }
    }

    pub fn remove_comments(&mut self) {
        self.data.retain(|s| {
            if let TokenType::Comment(_, _) = s.token() {
                return false;
            }
            true
        })
    }

    pub fn span(&self) -> Span {
        self.span_at(if self.pos() >= self.data.len() {
            self.pos().saturating_sub(1)
        } else {
            self.pos()
        })
        .expect("a span")
    }

    pub fn previous_span(&self) -> Span {
        self.span_at(self.pos().saturating_sub(1)).expect("a span")
    }

    pub fn span_at(&self, pos: usize) -> Option<Span> {
        self.data.get(pos).map(|s| s.span())
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    pub fn current(&self) -> Option<TokenType> {
        self.data.get(self.pos()).cloned().map(|s| s.token())
    }

    pub fn next_token(&mut self) -> Option<TokenType> {
        self.pos += 1;
        self.current()
    }

    pub fn peek(&self) -> Option<TokenType> {
        self.peek_ahead(1)
    }

    pub fn peek_ahead(&self, pos: usize) -> Option<TokenType> {
        self.data.get(self.pos + pos).cloned().map(|s| s.token())
    }
}

impl fmt::Debug for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Tokens")
            .field("position", &self.pos)
            .field("tokens", &self.data)
            .finish()
    }
}

impl fmt::Display for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", dump(&self.data))
    }
}

fn format_token(tok: &Token, width: usize) -> String {
    use self::TokenType::*;
    match tok.token() {
        Identifier(id) => format!("{} {: <width$}: Identifier", id, "", width = width),

        Integer(n) => format!("{} {: <width$}: Number", n, "", width = width),
        Real(n) => format!("{} {: <width$}: Real", n, "", width = width),
        String(s) => format!("'{}' {: <width$}: String", s, "", width = width),

        Comment(start, end) => format!("{},{} {: <width$}: Comment", start, end, "", width = width),
        LabelName(s) => format!("{} {: <width$}: Label", s, "", width = width),
        TypeName(ty) => format!("{:?} {: <width$}: Type", ty, "", width = width),

        Directive => format!("{} {: <width$}: Directive", "", "", width = width),
        Unknown => format!("{} {: <width$}: Unknown", "", "", width = width),

        EOF => "EOF".into(),

        s => format!("{} {: <width$}: Symbol", s, "", width = width),
    }
}

pub fn dump(s: &[Token]) -> String {
    use std::io::Cursor;

    let mut buf = Cursor::new(Vec::<u8>::new());
    dump_to(&mut buf, s);
    String::from_utf8(buf.into_inner()).expect(":valid utf-8")
}

pub fn dump_to<W: Write>(mut w: W, s: &[Token]) {
    use std::cmp::max;
    let (span_width, token_width) = s
        .iter()
        .map(|t| (t.span().total_width(), t.token().width()))
        .fold((0, 0), |mut a, (l, r)| {
            a.0 = max(a.0, l);
            a.1 = max(a.1, r);
            a
        });

    for (i, (span, tok)) in s.iter().map(|t| (t.span(), t)).enumerate() {
        let len = span_width - span.total_width();
        let tlen = token_width - tok.width();
        write!(
            w,
            "{}",
            format_args!("{}{: <width$}  ", span, "", width = len)
        )
        .expect("write");
        if i < s.len() - 1 {
            writeln!(w, "{}", format_token(&tok, tlen)).expect("writeln");
        } else {
            write!(w, "{}", format_token(&tok, tlen)).expect("write");
        }
    }
}
