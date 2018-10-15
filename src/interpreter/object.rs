use super::*;

#[derive(Debug, Clone)]
pub enum Object {
    Unit,
    Primitive(Primitive),
    Procedure(String, Vec<String>, Block),
    Function(String, Vec<String>, Block, Type),
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
    String(String),
    Boolean(bool),
}

impl Object {
    pub fn unary_plus(&self) -> Result<Self, Error> {
        match self {
            Object::Primitive(Primitive::Integer(i)) => {
                Ok(Object::Primitive(Primitive::Integer(*i)))
            }
            t => Err(Error::InvalidOperation(
                OperatorError::UnaryAdd,
                t.clone(),
                None,
            )),
        }
    }
    pub fn unary_minus(&self) -> Result<Self, Error> {
        match self {
            Object::Primitive(Primitive::Integer(i)) => {
                Ok(Object::Primitive(Primitive::Integer(-i)))
            }
            t => Err(Error::InvalidOperation(
                OperatorError::UnarySub,
                t.clone(),
                None,
            )),
        }
    }
    pub fn negate(&self) -> Result<Self, Error> {
        match self {
            Object::Primitive(Primitive::Boolean(i)) => {
                Ok(Object::Primitive(Primitive::Boolean(!i)))
            }
            t => Err(Error::InvalidOperation(
                OperatorError::UnaryNot,
                t.clone(),
                None,
            )),
        }
    }

    fn error<T>(left: &Self, right: &Self, op: OperatorError) -> Result<T, Error> {
        Err(Error::InvalidOperation(
            op,
            left.clone(),
            Some(right.clone()),
        ))
    }

    // binary
    pub fn add(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Integer(left + right)))
            }
            (Primitive(String(left)), Primitive(String(right))) => {
                Ok(Primitive(String(format!("{}{}", left, right))))
            }
            (left, right) => Self::error(left, right, OperatorError::Add),
        }
    }

    pub fn subtract(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Integer(left - right)))
            }
            (left, right) => Self::error(left, right, OperatorError::Sub),
        }
    }
    pub fn multiply(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Integer(left * right)))
            }
            (left, right) => Self::error(left, right, OperatorError::Mul),
        }
    }
    pub fn int_divide(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Integer(left / right)))
            }
            (left, right) => Self::error(left, right, OperatorError::IntDiv),
        }
    }

    pub fn and(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Boolean(left)), Primitive(Boolean(right))) => {
                Ok(Primitive(Boolean(*left && *right)))
            }
            (left, right) => Self::error(left, right, OperatorError::And),
        }
    }
    pub fn or(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Boolean(left)), Primitive(Boolean(right))) => {
                Ok(Primitive(Boolean(*left || *right)))
            }
            (left, right) => Self::error(left, right, OperatorError::Or),
        }
    }
    pub fn less_than(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Boolean(left < right)))
            }

            (left, right) => Self::error(left, right, OperatorError::LessThan),
        }
    }
    pub fn greater_than(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Boolean(left > right)))
            }

            (left, right) => Self::error(left, right, OperatorError::GreaterThan),
        }
    }
    pub fn less_than_equal(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Boolean(left <= right)))
            }

            (left, right) => Self::error(left, right, OperatorError::LessThanEqual),
        }
    }
    pub fn greater_than_equal(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Boolean(left)), Primitive(Boolean(right))) => {
                Ok(Primitive(Boolean(left >= right)))
            }

            (left, right) => Self::error(left, right, OperatorError::GreaterThanEqual),
        }
    }
    pub fn equal(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Boolean(left == right)))
            }

            (left, right) => Self::error(left, right, OperatorError::Equal),
        }
    }
    pub fn not_equal(&self, other: &Self) -> Result<Self, Error> {
        use self::{Object::Primitive, Primitive::*};

        match (self, other) {
            (Primitive(Integer(left)), Primitive(Integer(right))) => {
                Ok(Primitive(Boolean(left != right)))
            }

            (left, right) => Self::error(left, right, OperatorError::NotEqual),
        }
    }
}
