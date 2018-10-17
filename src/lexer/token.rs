use crate::count_digits;
use crate::lexer::span::Span;

use std::fmt;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    ty: TokenType,
    span: Span,
}

impl Token {
    pub(crate) fn new(span: Span, ty: TokenType) -> Self {
        Self { span, ty }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn token(&self) -> TokenType {
        self.ty.clone()
    }
}

impl Deref for Token {
    type Target = TokenType;
    fn deref(&self) -> &Self::Target {
        &self.ty
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    Integer,
    String,
    Boolean,
    Real,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Identifier(String),
    Integer(i64),
    Real(f64),
    String(String),
    TypeName(Type),
    LabelName(String),

    Comment(usize, usize), // start, end

    Directive,
    Unknown,
    EOF,

    Plus,
    Minus,
    Mul,
    IntDiv,
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

    True,
    False,
}

impl TokenType {
    pub(crate) fn try_parse(s: impl AsRef<str>) -> Option<Self> {
        use self::TokenType::*;

        let s = s.as_ref();
        let res = match s.to_ascii_lowercase().as_str() {
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

            "and" => And,
            "array" => Array,
            "begin" => Begin,
            "case" => Case,
            "const" => Const,
            "div" => IntDiv,
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

            "true" => True,
            "false" => False,
            _ => return None,
        };

        Some(res)
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::TokenType::*;
        let s = match self {
            Plus => "+",
            Minus => "-",
            Mul => "*",
            IntDiv => "/",
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

            Identifier(s) => return write!(f, "{} : Identifier", s),
            Integer(n) => return write!(f, "{} : Integer", n),
            Real(r) => return write!(f, "{} : Real", r),
            String(s) => return write!(f, "{} : String", s),
            TypeName(ty) => return write!(f, "{:?} : Type", ty),
            LabelName(l) => return write!(f, "{} : Label", l),

            _ => return write!(f, "{}", format!("{:?}", self)),
        };

        write!(f, "{}", s)
    }
}

/// This panics if its not a valid Symbol or Reserve
impl<'a> From<&'a str> for TokenType {
    fn from(s: &'a str) -> Self {
        let msg = || format!("can't turn '{}' into a TokenType", s);
        TokenType::try_parse(s).unwrap_or_else(|| panic!(msg()))
    }
}

impl TokenType {
    pub(crate) fn width(&self) -> usize {
        use self::TokenType::*;
        match self {
            Identifier(id) => id.len(),
            Integer(n) => count_digits(*n as usize),
            Real(n) => format!("{}", n).len(), // can't do this quickly
            String(s) => s.len() + 2,          // for ''
            TypeName(ty) => format!("{:?}", ty).len(),
            LabelName(s) => s.len(),
            Comment(start, end) => 1 + count_digits(*start) + count_digits(*end), // for the comma

            s => format!("{}", s).len(),
        }
    }
}
