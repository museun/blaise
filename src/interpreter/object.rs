use super::*;

use self::{Object::Primitive as Prim, Primitive::*};
use std::string::String as RString;

#[derive(Debug, Clone)]
pub enum Object {
    Unit,
    Primitive(Primitive),
    Procedure(RString, Vec<RString>, Block),
    Function(RString, Vec<RString>, Block, Type),
    Builtin(Builtin),
}

#[derive(Debug, Clone)]
pub enum Builtin {
    Write(fn(Object) -> Result<Object, Error>),
    WriteLn(fn(Object) -> Result<Object, Error>),
    ReadLn(fn() -> Result<Object, Error>),
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Integer(i32),
    String(RString),
    Boolean(bool),
}

impl Object {
    pub fn unary_plus(&self) -> Result<Self, Error> {
        match self {
            Prim(Integer(i)) => Ok(Prim(Integer(*i))),
            t => Err(Error::InvalidOperation(
                OperatorError::UnaryAdd,
                t.clone(),
                None,
            )),
        }
    }
    pub fn unary_minus(&self) -> Result<Self, Error> {
        match self {
            Prim(Integer(i)) => Ok(Prim(Integer(-i))),
            t => Err(Error::InvalidOperation(
                OperatorError::UnarySub,
                t.clone(),
                None,
            )),
        }
    }
    pub fn negate(&self) -> Result<Self, Error> {
        match self {
            Prim(Boolean(i)) => Ok(Prim(Boolean(!i))),
            t => Err(Error::InvalidOperation(
                OperatorError::UnaryNot,
                t.clone(),
                None,
            )),
        }
    }

    // binary
    pub fn add(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Integer(left + right))),
            (Prim(String(left)), Prim(String(right))) => {
                Ok(Prim(String(format!("{}{}", left, right))))
            }
            (left, right) => Self::error(left, right, OperatorError::Add),
        }
    }

    pub fn subtract(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Integer(left - right))),
            (left, right) => Self::error(left, right, OperatorError::Sub),
        }
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Integer(left * right))),
            (left, right) => Self::error(left, right, OperatorError::Mul),
        }
    }

    pub fn int_divide(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Integer(left / right))),
            (left, right) => Self::error(left, right, OperatorError::IntDiv),
        }
    }

    pub fn and(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Boolean(left)), Prim(Boolean(right))) => Ok(Prim(Boolean(*left && *right))),
            (left, right) => Self::error(left, right, OperatorError::And),
        }
    }

    pub fn or(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Boolean(left)), Prim(Boolean(right))) => Ok(Prim(Boolean(*left || *right))),
            (left, right) => Self::error(left, right, OperatorError::Or),
        }
    }

    pub fn less_than(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Boolean(left < right))),
            (left, right) => Self::error(left, right, OperatorError::LessThan),
        }
    }

    pub fn greater_than(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Boolean(left > right))),
            (left, right) => Self::error(left, right, OperatorError::GreaterThan),
        }
    }

    pub fn less_than_equal(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Boolean(left <= right))),
            (left, right) => Self::error(left, right, OperatorError::LessThanEqual),
        }
    }

    pub fn greater_than_equal(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Boolean(left)), Prim(Boolean(right))) => Ok(Prim(Boolean(left >= right))),
            (left, right) => Self::error(left, right, OperatorError::GreaterThanEqual),
        }
    }

    pub fn equal(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Boolean(left == right))),
            (left, right) => Self::error(left, right, OperatorError::Equal),
        }
    }

    pub fn not_equal(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok(Prim(Boolean(left != right))),
            (left, right) => Self::error(left, right, OperatorError::NotEqual),
        }
    }

    fn error<T>(left: &Self, right: &Self, op: OperatorError) -> Result<T, Error> {
        Err(Error::InvalidOperation(
            op,
            left.clone(),
            Some(right.clone()),
        ))
    }
}
