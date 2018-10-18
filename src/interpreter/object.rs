use crate::join_strings;
use super::*;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Object {
    Unit,
    Primitive(Primitive),
    Procedure(String, Vec<String>, Block),
    Function(String, Vec<String>, Block, Type),
    Variable(String, Type, Option<Box<Object>>),
    Builtin(Builtin),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Unit => write!(f, "Unit"),
            Object::Primitive(p) => write!(f, "{}", p),
            Object::Procedure(name, args, _block) => {
                write!(f, "proc {}({})", name, join_strings(&args, ", "))
            }
            Object::Function(name, args, _block, ty) => {
                write!(f, "fn {}({}) -> {:?}", name, join_strings(&args, ", "), ty)
            }
            Object::Variable(_name, _ty, Some(var)) => write!(f, "{}", var),
            Object::Variable(_name, ty, None) => write!(f, "{:?}", ty),
            Object::Builtin(b) => write!(f, "{:?} : Builtin", b),
        }
    }
}

#[derive(Clone)]
pub enum Builtin {
    Write(fn(&[Object]) -> Result<Object>),
    WriteLn(fn(&[Object]) -> Result<Object>),
    Read(fn(&[Object]) -> Result<Vec<(&String, Object)>>),
    ReadLn(fn(&[Object]) -> Result<Vec<(&String, Object)>>),
}

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::Write(_) => write!(f, "Write()"),
            Builtin::WriteLn(_) => write!(f, "WriteLn()"),
            Builtin::Read(_) => write!(f, "Read()"),
            Builtin::ReadLn(_) => write!(f, "ReadLn()"),
        }
    }
}

impl Object {
    pub fn unary_plus(&self) -> Result<Self> {
        if let Object::Primitive(p) = self {
            return Ok(p.unary_plus()?.into());
        }
        self.error(None, OperatorError::UnaryAdd)
    }

    pub fn unary_minus(&self) -> Result<Self> {
        if let Object::Primitive(p) = self {
            return Ok(p.unary_minus()?.into());
        }
        self.error(None, OperatorError::UnarySub)
    }

    pub fn negate(&self) -> Result<Self> {
        if let Object::Primitive(p) = self {
            return Ok(p.negate()?.into());
        }
        self.error(None, OperatorError::UnaryNot)
    }

    // binary
    pub fn add(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.add(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::Add),
        }
    }

    pub fn subtract(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.subtract(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::Sub),
        }
    }

    pub fn multiply(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.multiply(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::Mul),
        }
    }

    pub fn int_divide(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.int_divide(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::IntDiv),
        }
    }

    pub fn real_divide(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.real_divide(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::RealDiv),
        }
    }

    pub fn and(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.and(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::And),
        }
    }

    pub fn or(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.or(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::Or),
        }
    }

    pub fn less_than(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.less_than(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::LessThan),
        }
    }

    pub fn greater_than(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.greater_than(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::GreaterThan),
        }
    }

    pub fn less_than_equal(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.less_than_equal(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::LessThanEqual),
        }
    }

    pub fn greater_than_equal(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.greater_than_equal(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::GreaterThanEqual),
        }
    }

    pub fn equal(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.equal(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::Equal),
        }
    }

    pub fn not_equal(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Object::Primitive(l), Object::Primitive(r)) => Ok(l.not_equal(r)?.into()),
            (_, r) => self.error(Some(r), OperatorError::NotEqual),
        }
    }

    fn error<T>(&self, right: Option<&Self>, op: OperatorError) -> Result<T> {
        Err(Error::InvalidType(op, self.clone(), right.cloned()))
    }
}
