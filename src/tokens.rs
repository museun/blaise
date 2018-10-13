use super::span::Span;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Symbol(Symbol),
    Reserved(Reserved),

    Identifier(String),

    Number(i32),
    String(String),

    Type(Type),

    Comment(usize, usize), // start, end

    Label(String),
    Directive,
    Unknown,
    EOF,
}

impl Token {
    pub(crate) fn width(&self) -> usize {
        use self::Token::*;
        match self {
            Reserved(s) => format!("{}", s).len(),
            Symbol(s) => format!("{}", s).len(),
            Type(ty) => format!("{:?}", ty).len(),

            Identifier(id) => id.len(),
            Number(n) => count_digits(*n as usize),
            String(s) => s.len(),
            Label(s) => s.len(),
            Comment(start, end) => 1 + count_digits(*start) + count_digits(*end), // for the comma
            _ => 0,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    Integer,
    String,
    Bool,
    // Real, Unit
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Reserved {
    And,
    Array,
    Begin,
    Case,
    Const,
    Div,
    Do,
    Downto,
    Else,
    End,
    File,
    For,
    Function,
    Goto,
    If,
    In,
    Label,
    Mod,
    Nil,
    Not,
    Of,
    Or,
    Packed,
    Procedure,
    Program,
    Record,
    Repeat,
    Set,
    Then,
    To,
    Type,
    Until,
    Var,
    While,
    With,
}

impl Reserved {
    pub fn new(s: &str) -> Option<Self> {
        use self::Reserved::*;
        let res = match s.to_ascii_lowercase().as_str() {
            "and" => And,
            "array" => Array,
            "begin" => Begin,
            "case" => Case,
            "const" => Const,
            "div" => Div,
            "do" => Do,
            "downto" => Downto,
            "else" => Else,
            "end" => End,
            "file" => File,
            "for" => For,
            "function" => Function,
            "goto" => Goto,
            "if" => If,
            "in" => In,
            "label" => Label,
            "mod" => Mod,
            "nil" => Nil,
            "not" => Not,
            "of" => Of,
            "or" => Or,
            "packed" => Packed,
            "procedure" => Procedure,
            "program" => Program,
            "record" => Record,
            "repeat" => Repeat,
            "set" => Set,
            "then" => Then,
            "to" => To,
            "type" => Type,
            "until" => Until,
            "var" => Var,
            "while" => While,
            "with" => With,
            _ => return None,
        };
        Some(res)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Symbol {
    Plus,
    Minus,
    Mul,
    Div,
    Equal,
    LessThan,
    GreaterThan,
    OpenBracket,
    CloseBracket,
    Period,
    Comma,
    Colon,
    SemiColon,
    UpArrow,
    OpenParen,
    CloseParen,
    NotEqual,
    LessThanEqual,
    GreaterThanEqual,
    Assign,
    SubRange,
}

impl Symbol {
    pub fn new(s: &str) -> Option<Self> {
        use self::Symbol::*;
        let res = match s {
            "+" => Plus,
            "-" => Minus,
            "*" => Mul,
            "/" => Div,
            "=" => Equal,
            "<" => LessThan,
            ">" => GreaterThan,
            "[" => OpenBracket,
            "]" => CloseBracket,
            "." => Period,
            "," => Comma,
            ":" => Colon,
            ";" => SemiColon,
            "^" => UpArrow,
            "(" => OpenParen,
            ")" => CloseParen,
            "<>" => NotEqual,
            "<=" => LessThanEqual,
            ">=" => GreaterThanEqual,
            ":=" => Assign,
            ".." => SubRange,
            _ => return None,
        };
        Some(res)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Symbol::*;
        let s = match self {
            Plus => "+",
            Minus => "-",
            Mul => "*",
            Div => "/",
            Equal => "=",
            LessThan => "<",
            GreaterThan => ">",
            OpenBracket => "[",
            CloseBracket => "]",
            Period => ".",
            Comma => ",",
            Colon => ":",
            SemiColon => ";",
            UpArrow => "^",
            OpenParen => "(",
            CloseParen => ")",
            NotEqual => "<>",
            LessThanEqual => "<=",
            GreaterThanEqual => ">=",
            Assign => ":=",
            SubRange => "..",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for Reserved {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_ascii_lowercase())
    }
}

pub struct Tokens<'a> {
    data: Vec<(Span<'a>, Token)>,
    pos: usize,
}

impl<'a> fmt::Debug for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Tokens")
            .field("position", &self.pos)
            .field("tokens", &self.data)
            .finish()
    }
}

impl<'a> Tokens<'a> {
    pub fn new(data: Vec<(Span<'a>, Token)>) -> Self {
        Self { data, pos: 0 }
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

    pub fn current(&self) -> Option<&(Span<'a>, Token)> {
        self.data.get(self.pos - 1)
    }

    pub fn peek(&mut self) -> Option<&(Span<'a>, Token)> {
        self.data.get(self.pos)
    }

    pub fn peek_ahead(&mut self, pos: usize) -> Option<&(Span<'a>, Token)> {
        self.data.get(self.pos + pos)
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = (Span<'a>, Token);
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.is_empty() || self.data.len() == self.pos() {
            return None;
        }

        let n = self.data.get(self.pos()).cloned();
        self.pos += 1;
        n
    }
}

impl<'a> ExactSizeIterator for Tokens<'a> {}

impl<'a> fmt::Display for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::cmp::max;

        let (span_width, token_width) = self
            .data
            .iter()
            .map(|(span, token)| (span.total_width(), token.width()))
            .fold((0, 0), |mut a, (l, r)| {
                a.0 = max(a.0, l);
                a.1 = max(a.1, r);
                a
            });

        fn format_token(tok: &Token, width: usize) -> String {
            use self::Token::*;

            match tok {
                Reserved(s) => format!("{} {: <width$}: Symbol", s, "", width = width),
                Symbol(s) => format!("{} {: <width$}: Symbol", s, "", width = width),

                Identifier(id) => format!("{} {: <width$}: Identifier", id, "", width = width),
                Number(n) => format!("{} {: <width$}: Number", n, "", width = width),
                String(s) => format!("{} {: <width$}: String", s, "", width = width),
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

        for (span, tok) in self.data.iter() {
            let len = span_width - span.total_width();
            let tlen = token_width - tok.width();
            write!(
                f,
                "{}",
                format_args!("{}{: <width$}  ", span, "", width = len)
            )?;
            writeln!(f, "{}", format_token(tok, tlen))?;
        }

        Ok(())
    }
}

#[inline(always)]
fn count_digits(mut n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut x = 0;
    while n > 0 {
        n /= 10;
        x += 1;
    }
    x
}
