use crate::prelude::*;

mod error;
use self::ast::*;

#[allow(clippy::module_inception)]
mod parser;
pub use self::parser::Parser;

pub mod ast;
pub use self::error::{Error, ErrorKind};

type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Call = 7,
    UnaryLiteral = 6,
    BinaryMul = 5,
    BinaryAdd = 4,
    Relative = 3,
    UnaryBool = 2,
    BinaryBool = 1,
    None = 0,
}
