use crate::count_digits;
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
    pub(crate) fn try_parse(s: impl AsRef<str>) -> Option<Self> {
        let s = s.as_ref();
        if s.len() <= 2 {
            if let Some(s) = Symbol::new(s) {
                return Some(Token::Symbol(s));
            }
        }
        if let Some(s) = Reserved::new(s) {
            return Some(Token::Reserved(s));
        }
        None
    }
}

/// This panics if its not a valid Symbol or Reserve
impl<'a> From<&'a str> for Token {
    fn from(s: &'a str) -> Self {
        let msg = || format!("can't turn '{}' into a Token", s);
        Token::try_parse(s).expect(&msg())
    }
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
    Boolean,
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
