use self::Primitive::*;
use super::*;

use std::fmt;

#[derive(Debug, Clone)]
pub enum Primitive {
    Integer(i64),
    Str(String),
    Boolean(bool),
    Real(f64),
}

impl Primitive {
    pub fn get_type(&self) -> Type {
        match self {
            Primitive::Integer(_) => Type::Integer,
            Primitive::Str(_) => Type::String,
            Primitive::Boolean(_) => Type::Boolean,
            Primitive::Real(_) => Type::Real,
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Integer(n) => write!(f, "{} : Integer", n),
            Primitive::Str(n) => write!(f, "{} : String", n),
            Primitive::Boolean(n) => write!(f, "{} : Boolean", n),
            Primitive::Real(n) => write!(f, "{} : Real", n),
        }
    }
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

impl From<String> for Object {
    fn from(s: String) -> Self {
        Object::Primitive(Primitive::Str(s))
    }
}

impl From<bool> for Object {
    fn from(s: bool) -> Self {
        Object::Primitive(Primitive::Boolean(s))
    }
}

impl From<i64> for Primitive {
    fn from(s: i64) -> Self {
        Primitive::Integer(s)
    }
}

impl From<f64> for Primitive {
    fn from(r: f64) -> Self {
        Primitive::Real(r)
    }
}

impl From<String> for Primitive {
    fn from(s: String) -> Self {
        Primitive::Str(s)
    }
}

impl From<bool> for Primitive {
    fn from(s: bool) -> Self {
        Primitive::Boolean(s)
    }
}

impl From<Primitive> for Object {
    fn from(s: Primitive) -> Self {
        Object::Primitive(s)
    }
}

impl Primitive {
    pub fn unary_plus(&self) -> Result<Self> {
        match self {
            Integer(i) => Ok(Integer(*i)),
            Real(i) => Ok(Real(*i)),
            left => Self::error(left, None, OperatorError::UnaryAdd),
        }
    }

    pub fn unary_minus(&self) -> Result<Self> {
        match self {
            Integer(i) => Ok(Integer(-i)),
            Real(i) => Ok(Real(-i)),
            left => Self::error(left, None, OperatorError::UnarySub),
        }
    }

    pub fn negate(&self) -> Result<Self> {
        match self {
            Boolean(i) => Ok(Boolean(!i)),
            left => Self::error(left, None, OperatorError::UnaryNot),
        }
    }

    pub fn add(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left + right).into(),

            (Real(left), Real(right)) => (left + right).into(),
            (Integer(left), Real(right)) => ((*left as f64) + (*right as f64)).into(),
            (Real(left), Integer(right)) => ((*left as f64) + (*right as f64)).into(),

            (Str(left), Str(right)) => format!("{}{}", left, right).into(),
            (left, right) => Self::error(left, Some(right), OperatorError::Add)?,
        };
        Ok(res)
    }

    pub fn subtract(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left - right).into(),

            (Real(left), Real(right)) => (left - right).into(),
            (Integer(left), Real(right)) => ((*left as f64) - (*right as f64)).into(),
            (Real(left), Integer(right)) => ((*left as f64) - (*right as f64)).into(),

            (left, right) => Self::error(left, Some(right), OperatorError::Sub)?,
        };
        Ok(res)
    }

    pub fn multiply(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left * right).into(),

            (Real(left), Real(right)) => (left * right).into(),
            (Integer(left), Real(right)) => ((*left as f64) * (*right as f64)).into(),
            (Real(left), Integer(right)) => ((*left as f64) * (*right as f64)).into(),

            (left, right) => Self::error(left, Some(right), OperatorError::Mul)?,
        };
        Ok(res)
    }

    pub fn int_divide(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left / right).into(),
            (left, right) => Self::error(left, Some(right), OperatorError::IntDiv)?,
        };
        Ok(res)
    }

    pub fn real_divide(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Real(left), Real(right)) => ((*left as f64) / (*right as f64)).into(),
            (Integer(left), Integer(right)) => ((*left as f64) / (*right as f64)).into(),
            (Real(left), Integer(right)) => ((*left as f64) / (*right as f64)).into(),
            (Integer(left), Real(right)) => ((*left as f64) / (*right as f64)).into(),

            (left, right) => Self::error(left, Some(right), OperatorError::RealDiv)?,
        };
        Ok(res)
    }

    pub fn and(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Boolean(left), Boolean(right)) => (*left && *right).into(),
            (left, right) => Self::error(left, Some(right), OperatorError::And)?,
        };
        Ok(res)
    }

    pub fn or(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Boolean(left), Boolean(right)) => (*left || *right).into(),
            (left, right) => Self::error(left, Some(right), OperatorError::Or)?,
        };
        Ok(res)
    }

    pub fn less_than(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left < right).into(),

            (Real(left), Real(right)) => (left < right).into(),
            (Integer(left), Real(right)) => ((*left as f64) < (*right as f64)).into(),
            (Real(left), Integer(right)) => ((*left as f64) < (*right as f64)).into(),

            (left, right) => Self::error(left, Some(right), OperatorError::LessThan)?,
        };
        Ok(res)
    }

    pub fn greater_than(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left > right).into(),

            (Real(left), Real(right)) => (left > right).into(),
            (Integer(left), Real(right)) => ((*left as f64) > (*right as f64)).into(),
            (Real(left), Integer(right)) => ((*left as f64) > (*right as f64)).into(),

            (left, right) => Self::error(left, Some(right), OperatorError::GreaterThan)?,
        };
        Ok(res)
    }

    pub fn less_than_equal(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left <= right).into(),

            (Real(left), Real(right)) => (left <= right).into(),
            (Integer(left), Real(right)) => ((*left as f64) <= (*right as f64)).into(),
            (Real(left), Integer(right)) => ((*left as f64) <= (*right as f64)).into(),

            (left, right) => Self::error(left, Some(right), OperatorError::LessThanEqual)?,
        };
        Ok(res)
    }

    pub fn greater_than_equal(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Boolean(left), Boolean(right)) => (left >= right).into(),

            (Real(left), Real(right)) => (left >= right).into(),
            (Integer(left), Real(right)) => ((*left as f64) >= (*right as f64)).into(),
            (Real(left), Integer(right)) => ((*left as f64) >= (*right as f64)).into(),

            (left, right) => Self::error(left, Some(right), OperatorError::GreaterThanEqual)?,
        };
        Ok(res)
    }

    pub fn equal(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left == right).into(),

            // (Real(left), Real(right)) => (left == right).into(),
            // (Integer(left), Real(right)) => {
            //     ((*left as f64) == (*right as f64)).into()
            // }
            // (Real(left), Integer(right)) => {
            //     ((*left as f64) == (*right as f64)).into()
            // }
            (left, right) => Self::error(left, Some(right), OperatorError::Equal)?,
        };
        Ok(res)
    }

    pub fn not_equal(&self, other: &Self) -> Result<Self> {
        let res = match (self, other) {
            (Integer(left), Integer(right)) => (left != right).into(),

            // (Real(left), Real(right)) => (left != right).into(),
            // (Integer(left), Real(right)) => {
            //     ((*left as f64) != (*right as f64)).into()
            // }
            // (Real(left), Integer(right)) => {
            //     ((*left as f64) != (*right as f64)).into()
            // }
            (left, right) => Self::error(left, Some(right), OperatorError::NotEqual)?,
        };
        Ok(res)
    }

    fn error<T>(left: &Self, right: Option<&Self>, op: OperatorError) -> Result<T> {
        Err(Error::InvalidOperation(op, left.clone(), right.cloned()))
    }
}
