use super::span::Span;
use super::token::Token;

use std::fmt;
use std::io::prelude::*;

#[derive(Clone)]
pub struct Tokens {
    data: Vec<(Span, Token)>,
    pos: usize,
    source: String,
}

impl Tokens {
    pub fn new(data: Vec<(Span, Token)>, source: String) -> Self {
        Self {
            data,
            pos: 0,
            source,
        }
    }

    pub fn remaining(&self) -> &[(Span, Token)] {
        &self.data[self.pos()..]
    }

    pub fn tokens(&self) -> &[(Span, Token)] {
        &self.data
    }

    pub fn source(&self) -> &str {
        self.source.as_str()
    }

    pub fn span(&self) -> &Span {
        self.span_at(self.pos()).expect("a span")
    }

    pub fn span_at(&self, pos: usize) -> Option<&Span> {
        self.data.get(pos).map(|(s, _)| s)
    }

    pub fn remove_comments(&mut self) {
        use std::mem::discriminant;
        let comment = discriminant(&Token::Comment(0, 0));
        self.data.retain(|(_, t)| discriminant(t) != comment)
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

    pub fn current(&self) -> Token {
        self.data
            .get(self.pos.checked_sub(1).unwrap_or(0))
            .map(|(_, t)| t)
            .cloned()
            .expect("not eof")
    }

    pub fn next_token(&mut self) -> Token {
        let n = self
            .data
            .get(self.pos())
            .cloned()
            .map(|(_, t)| t)
            .expect("not eof");
        self.pos += 1;
        n
    }

    pub fn peek(&mut self) -> Token {
        self.data
            .get(self.pos)
            .map(|(_, t)| t)
            .cloned()
            .expect("not eof")
    }

    pub fn peek_ahead(&mut self, pos: usize) -> Option<Token> {
        self.data.get(self.pos + pos).map(|(_, t)| t).cloned()
    }

    pub fn dump(&self, s: &[(Span, Token)]) -> String {
        use std::io::Cursor;
        let mut buf = Cursor::new(Vec::<u8>::new());
        self.dump_to(&mut buf, s);
        String::from_utf8(buf.into_inner()).expect(":valid utf-8")
    }

    pub fn dump_to<W: Write>(&self, mut w: W, s: &[(Span, Token)]) {
        fn format_token(tok: &Token, width: usize) -> String {
            use self::Token::*;
            match tok {
                Reserved(s) => format!("{} {: <width$}: Symbol", s, "", width = width),
                Symbol(s) => format!("{} {: <width$}: Symbol", s, "", width = width),

                Identifier(id) => format!("{} {: <width$}: Identifier", id, "", width = width),

                Number(n) => format!("{} {: <width$}: Number", n, "", width = width),
                Real(n) => format!("{} {: <width$}: Real", n, "", width = width),
                String(s) => format!("'{}' {: <width$}: String", s, "", width = width),

                Comment(start, end) => {
                    format!("{},{} {: <width$}: Comment", start, end, "", width = width)
                }
                Label(s) => format!("{} {: <width$}: Label", s, "", width = width),
                Type(ty) => format!("{:?} {: <width$}: Type", ty, "", width = width),

                Directive => format!("{} {: <width$}: Directive", "", "", width = width),
                Unknown => format!("{} {: <width$}: Unknown", "", "", width = width),

                EOF => "EOF".into(),
            }
        }

        use std::cmp::max;
        let (span_width, token_width) = s
            .iter()
            .map(|(span, token)| (span.total_width(), token.width()))
            .fold((0, 0), |mut a, (l, r)| {
                a.0 = max(a.0, l);
                a.1 = max(a.1, r);
                a
            });

        for (span, tok) in s {
            let len = span_width - span.total_width();
            let tlen = token_width - tok.width();
            write!(
                w,
                "{}",
                format_args!("{}{: <width$}  ", span, "", width = len)
            )
            .expect("write");
            writeln!(w, "{}", format_token(&tok, tlen)).expect("writeln");
        }
    }
}

impl Iterator for Tokens {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.is_empty() || self.data.len() == self.pos() {
            return None;
        }

        let n = self.data.get(self.pos()).cloned().map(|(_, t)| t);
        self.pos += 1;
        n
    }
}

impl ExactSizeIterator for Tokens {}

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
        writeln!(f, "{}", self.dump(&self.data))
    }
}
