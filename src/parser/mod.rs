use crate::prelude::*;

mod error;
// mod infix;
// mod prefix;

use self::ast::*;
// use self::infix::*;
// use self::prefix::*;

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

pub(crate) trait BinaryOp {
    fn as_binary_op(&self) -> Option<BinaryOperator>;
}

impl BinaryOp for TokenType {
    fn as_binary_op(&self) -> Option<BinaryOperator> {
        let res = match self {
            TokenType::Plus => BinaryOperator::Plus,
            TokenType::Minus => BinaryOperator::Minus,
            TokenType::Mul => BinaryOperator::Mul,
            TokenType::Div => BinaryOperator::Div,
            TokenType::IntDiv => BinaryOperator::IntDiv,

            TokenType::LessThan => BinaryOperator::LessThan,
            TokenType::GreaterThan => BinaryOperator::GreaterThan,
            TokenType::LessThanEqual => BinaryOperator::LessThanEqual,
            TokenType::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
            TokenType::Equal => BinaryOperator::Equal,
            TokenType::NotEqual => BinaryOperator::NotEqual,

            TokenType::And => BinaryOperator::And,
            TokenType::Or => BinaryOperator::Or,
            _ => return None,
        };

        Some(res)
    }
}
