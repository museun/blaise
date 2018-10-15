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
    Write(fn(Object) -> Result<Object>),
    WriteLn(fn(Object) -> Result<Object>),
    ReadLn(fn() -> Result<Object>),
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Integer(i64),
    String(RString),
    Boolean(bool),
    Real(f64),
}

impl From<i64> for Object {
    fn from(s: i64) -> Self {
        Object::Primitive(Primitive::Integer(s))
    }
}

impl From<f64> for Object {
    fn from(r: f64) -> Self {
        Object::Primitive(Primitive::Real(r))
    }
}

impl From<RString> for Object {
    fn from(s: RString) -> Self {
        Object::Primitive(Primitive::String(s))
    }
}

impl From<bool> for Object {
    fn from(s: bool) -> Self {
        Object::Primitive(Primitive::Boolean(s))
    }
}

impl Object {
    pub fn unary_plus(&self) -> Result<Self> {
        match self {
            Prim(Integer(i)) => Ok((*i).into()),
            Prim(Real(i)) => Ok((*i).into()),
            left => Self::error(left, None, OperatorError::UnaryAdd),
        }
    }

    pub fn unary_minus(&self) -> Result<Self> {
        match self {
            Prim(Integer(i)) => Ok((-i).into()),
            Prim(Real(i)) => Ok((-i).into()),
            left => Self::error(left, None, OperatorError::UnarySub),
        }
    }

    pub fn negate(&self) -> Result<Self> {
        match self {
            Prim(Boolean(i)) => Ok((!i).into()),
            left => Self::error(left, None, OperatorError::UnaryNot),
        }
    }

    // binary
    pub fn add(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left + right).into()),

            (Prim(Real(left)), Prim(Real(right))) => Ok((left + right).into()),
            (Prim(Integer(left)), Prim(Real(right))) => {
                Ok(((*left as f64) + (*right as f64)).into())
            }
            (Prim(Real(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) + (*right as f64)).into())
            }

            (Prim(String(left)), Prim(String(right))) => Ok(format!("{}{}", left, right).into()),
            (left, right) => Self::error(left, Some(right), OperatorError::Add),
        }
    }

    pub fn subtract(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left - right).into()),

            (Prim(Real(left)), Prim(Real(right))) => Ok((left - right).into()),
            (Prim(Integer(left)), Prim(Real(right))) => {
                Ok(((*left as f64) - (*right as f64)).into())
            }
            (Prim(Real(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) - (*right as f64)).into())
            }

            (left, right) => Self::error(left, Some(right), OperatorError::Sub),
        }
    }

    pub fn multiply(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left * right).into()),

            (Prim(Real(left)), Prim(Real(right))) => Ok((left * right).into()),
            (Prim(Integer(left)), Prim(Real(right))) => {
                Ok(((*left as f64) * (*right as f64)).into())
            }
            (Prim(Real(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) * (*right as f64)).into())
            }

            (left, right) => Self::error(left, Some(right), OperatorError::Mul),
        }
    }

    pub fn int_divide(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left / right).into()),
            (left, right) => Self::error(left, Some(right), OperatorError::IntDiv),
        }
    }

    // real
    pub fn real_divide(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Real(left)), Prim(Real(right))) => Ok(((*left as f64) / (*right as f64)).into()),
            (Prim(Integer(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) / (*right as f64)).into())
            }
            (Prim(Real(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) / (*right as f64)).into())
            }
            (Prim(Integer(left)), Prim(Real(right))) => {
                Ok(((*left as f64) / (*right as f64)).into())
            }

            (left, right) => Self::error(left, Some(right), OperatorError::RealDiv),
        }
    }
    // real

    pub fn and(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Boolean(left)), Prim(Boolean(right))) => Ok((*left && *right).into()),
            (left, right) => Self::error(left, Some(right), OperatorError::And),
        }
    }

    pub fn or(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Boolean(left)), Prim(Boolean(right))) => Ok((*left || *right).into()),
            (left, right) => Self::error(left, Some(right), OperatorError::Or),
        }
    }

    pub fn less_than(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left < right).into()),

            (Prim(Real(left)), Prim(Real(right))) => Ok((left < right).into()),
            (Prim(Integer(left)), Prim(Real(right))) => {
                Ok(((*left as f64) < (*right as f64)).into())
            }
            (Prim(Real(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) < (*right as f64)).into())
            }

            (left, right) => Self::error(left, Some(right), OperatorError::LessThan),
        }
    }

    pub fn greater_than(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left > right).into()),

            (Prim(Real(left)), Prim(Real(right))) => Ok((left > right).into()),
            (Prim(Integer(left)), Prim(Real(right))) => {
                Ok(((*left as f64) > (*right as f64)).into())
            }
            (Prim(Real(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) > (*right as f64)).into())
            }

            (left, right) => Self::error(left, Some(right), OperatorError::GreaterThan),
        }
    }

    pub fn less_than_equal(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left <= right).into()),

            (Prim(Real(left)), Prim(Real(right))) => Ok((left <= right).into()),
            (Prim(Integer(left)), Prim(Real(right))) => {
                Ok(((*left as f64) <= (*right as f64)).into())
            }
            (Prim(Real(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) <= (*right as f64)).into())
            }

            (left, right) => Self::error(left, Some(right), OperatorError::LessThanEqual),
        }
    }

    pub fn greater_than_equal(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Boolean(left)), Prim(Boolean(right))) => Ok((left >= right).into()),

            (Prim(Real(left)), Prim(Real(right))) => Ok((left >= right).into()),
            (Prim(Integer(left)), Prim(Real(right))) => {
                Ok(((*left as f64) >= (*right as f64)).into())
            }
            (Prim(Real(left)), Prim(Integer(right))) => {
                Ok(((*left as f64) >= (*right as f64)).into())
            }

            (left, right) => Self::error(left, Some(right), OperatorError::GreaterThanEqual),
        }
    }

    pub fn equal(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left == right).into()),

            // (Prim(Real(left)), Prim(Real(right))) => Ok((left == right).into()),
            // (Prim(Integer(left)), Prim(Real(right))) => {
            //     Ok(((*left as f64) == (*right as f64)).into())
            // }
            // (Prim(Real(left)), Prim(Integer(right))) => {
            //     Ok(((*left as f64) == (*right as f64)).into())
            // }
            (left, right) => Self::error(left, Some(right), OperatorError::Equal),
        }
    }

    pub fn not_equal(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Prim(Integer(left)), Prim(Integer(right))) => Ok((left != right).into()),

            // (Prim(Real(left)), Prim(Real(right))) => Ok((left != right).into()),
            // (Prim(Integer(left)), Prim(Real(right))) => {
            //     Ok(((*left as f64) != (*right as f64)).into())
            // }
            // (Prim(Real(left)), Prim(Integer(right))) => {
            //     Ok(((*left as f64) != (*right as f64)).into())
            // }
            (left, right) => Self::error(left, Some(right), OperatorError::NotEqual),
        }
    }

    fn error<T>(left: &Self, right: Option<&Self>, op: OperatorError) -> Result<T> {
        Err(Error::InvalidOperation(op, left.clone(), right.cloned()))
    }
}
