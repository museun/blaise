use crate::prelude::*;

mod error;
mod infix;
mod prefix;

use self::ast::*;
use self::infix::*;
use self::prefix::*;

#[allow(clippy::module_inception)]
mod parser;
pub use self::parser::Parser;

pub mod ast;
pub use self::error::{Error, ErrorKind};

type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Precedence {
    Call = 7,
    UnaryLiteral = 6,
    BinaryMul = 5,
    BinaryAdd = 4,
    Relative = 3,
    UnaryBool = 2,
    BinaryBool = 1,
}

pub(crate) trait BinaryOp {
    fn as_binary_op(&self) -> Option<BinaryOperator>;
}

impl BinaryOp for Symbol {
    fn as_binary_op(&self) -> Option<BinaryOperator> {
        let res = match self {
            Symbol::Plus => BinaryOperator::Plus,
            Symbol::Minus => BinaryOperator::Minus,
            Symbol::Mul => BinaryOperator::Mul,
            Symbol::Div => BinaryOperator::RealDiv,

            Symbol::LessThan => BinaryOperator::LessThan,
            Symbol::GreaterThan => BinaryOperator::GreaterThan,
            Symbol::LessThanEqual => BinaryOperator::LessThanEqual,
            Symbol::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
            Symbol::Equal => BinaryOperator::Equal,
            Symbol::NotEqual => BinaryOperator::NotEqual,
            _ => return None,
        };
        Some(res)
    }
}

impl BinaryOp for Reserved {
    fn as_binary_op(&self) -> Option<BinaryOperator> {
        Some(match self {
            Reserved::And => BinaryOperator::And,
            Reserved::Or => BinaryOperator::Or,
            Reserved::Div => BinaryOperator::Div,
            _ => return None,
        })
    }
}
