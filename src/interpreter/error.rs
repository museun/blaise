use super::*;

#[derive(Debug)]
pub enum Error {
    CannotRead,
    UnknownVariable(String),
    UnknownScope,
    UnknownFunction(String), // Procedure?
    InvalidArgument,
    InvalidType(OperatorError, Object, Option<Object>),
    InvalidOperation(OperatorError, Primitive, Option<Primitive>),
}

#[derive(Debug)]
pub enum OperatorError {
    Add,
    Sub,
    Mul,
    IntDiv,
    RealDiv,
    UnaryAdd,
    UnarySub,
    UnaryNot,
    And,
    Or,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equal,
    NotEqual,
}
