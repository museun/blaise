use crate::ast::*;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
enum Error {
    UnknownVariable(String),
    UnknownScope,

    CannotRead,

    InvalidAdd(Object, Object),
    InvalidSub(Object, Object),
    InvalidMul(Object, Object),
    InvalidIntDiv(Object, Object),

    InvalidUnaryAdd(Object),
    InvalidUnarySub(Object),
    InvalidUnaryNot(Object),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Default)]
pub struct Interpreter {
    scope: Option<Scope>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn evaluate(&mut self, program: &Program) {
        if let Err(err) = self.visit(program) {
            eprintln!("{}", err);
            eprintln!("{:#?}", self.scope);
            ::std::process::exit(1)
        }
    }

    fn visit(&mut self, node: &Program) -> Result<(), Error> {
        self.enter(((node.0).0).clone());
        self.init()?;
        eprintln!("{:#?}", self.visit_block(&node.1)?);
        self.leave();
        Ok(())
    }

    fn visit_block(&mut self, node: &Block) -> Result<Object, Error> {
        self.visit_declarations(&node.0)?;
        self.visit_compound(&node.1)
    }

    fn visit_declarations(&mut self, node: &[Declaration]) -> Result<(), Error> {
        for decl in node {
            match decl {
                _ => {}
            }
        }
        Ok(())
    }

    fn visit_compound(&mut self, node: &Compound) -> Result<Object, Error> {
        let mut obj = Object::Unit;
        for statement in &node.0 {
            obj = self.visit_statement(statement)?;
        }
        Ok(obj)
    }

    fn visit_statement(&mut self, node: &Statement) -> Result<Object, Error> {
        match node {
            Statement::Compound(comp) => self.visit_compound(comp),
            Statement::Assignment(assign) => self.visit_assignment(assign),
        }
    }

    fn visit_assignment(&mut self, node: &Assignment) -> Result<Object, Error> {
        let val = self.visit_expression(&node.1)?;
        self.scope()?.set((node.0).0.clone(), val.clone());
        Ok(val)
    }

    fn visit_expression(&mut self, node: &Expression) -> Result<Object, Error> {
        match node {
            Expression::Unary(expr) => self.visit_unary_op(expr),
            Expression::Binary(expr) => self.visit_binary_op(expr),
            Expression::Literal(expr) => self.visit_literal(expr),
            Expression::Variable(expr) => self.visit_variable(expr),
        }
    }

    fn visit_unary_op(&mut self, node: &UnaryExpression) -> Result<Object, Error> {
        match node {
            UnaryExpression(UnaryOperator::Plus, expr) => self.visit_expression(expr)?.unary_plus(),
            UnaryExpression(UnaryOperator::Minus, expr) => {
                self.visit_expression(expr)?.unary_minus()
            }
            UnaryExpression(UnaryOperator::Not, expr) => self.visit_expression(expr)?.negate(),
        }
    }

    fn visit_binary_op(&mut self, node: &BinaryExpression) -> Result<Object, Error> {
        match node {
            BinaryExpression(left, BinaryOperator::Plus, expr) => self
                .visit_expression(left)?
                .add(&self.visit_expression(expr)?),
            BinaryExpression(left, BinaryOperator::Minus, expr) => self
                .visit_expression(left)?
                .subtract(&self.visit_expression(expr)?),
            BinaryExpression(left, BinaryOperator::Mul, expr) => self
                .visit_expression(left)?
                .multiply(&self.visit_expression(expr)?),
            BinaryExpression(left, BinaryOperator::Div, expr) => self
                .visit_expression(left)?
                .int_divide(&self.visit_expression(expr)?),
        }
    }

    fn visit_literal(&mut self, node: &Literal) -> Result<Object, Error> {
        Ok(match node {
            Literal::Integer(n) => Object::Primitive(Primitive::Integer(*n)),
            Literal::String(s) => Object::Primitive(Primitive::String(s.clone())),
        })
    }

    fn visit_variable(&mut self, node: &Variable) -> Result<Object, Error> {
        match self.scope()?.get(&node.0) {
            Some(object) => Ok(object.clone()),
            None => Err(Error::UnknownVariable(node.0.clone())),
        }
    }

    fn init(&mut self) -> Result<(), Error> {
        let _ = self.scope()?;
        Ok(())
    }

    fn enter(&mut self, name: impl Into<String>) {
        self.scope = match self.scope.take() {
            Some(scope) => Some(Scope::with_parent(name, scope)),
            None => Some(Scope::new(name.into())),
        }
    }

    fn leave(&mut self) {
        self.scope = match self.scope.take() {
            Some(scope) => scope.parent(),
            None => None,
        }
    }

    fn scope(&mut self) -> Result<&mut Scope, Error> {
        match self.scope {
            Some(ref mut scope) => Ok(scope),
            None => Err(Error::UnknownScope),
        }
    }
}

#[derive(Debug, Clone)]
enum Object {
    Unit,
    Primitive(Primitive),
}

#[derive(Debug, Clone)]
enum Primitive {
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
            t => Err(Error::InvalidUnaryAdd(t.clone())),
        }
    }
    pub fn unary_minus(&self) -> Result<Self, Error> {
        match self {
            Object::Primitive(Primitive::Integer(i)) => {
                Ok(Object::Primitive(Primitive::Integer(-i)))
            }
            t => Err(Error::InvalidUnarySub(t.clone())),
        }
    }
    pub fn negate(&self) -> Result<Self, Error> {
        match self {
            Object::Primitive(Primitive::Boolean(i)) => {
                Ok(Object::Primitive(Primitive::Boolean(!i)))
            }
            t => Err(Error::InvalidUnaryNot(t.clone())),
        }
    }

    // binary
    pub fn add(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Integer(left + right))),
            (
                Object::Primitive(Primitive::String(left)),
                Object::Primitive(Primitive::String(right)),
            ) => Ok(Object::Primitive(Primitive::String(format!(
                "{}{}",
                left, right
            )))),
            (left, right) => Err(Error::InvalidAdd(left.clone(), right.clone())),
        }
    }
    pub fn subtract(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Integer(left - right))),
            (left, right) => Err(Error::InvalidSub(left.clone(), right.clone())),
        }
    }
    pub fn multiply(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Integer(left * right))),
            (left, right) => Err(Error::InvalidMul(left.clone(), right.clone())),
        }
    }
    pub fn int_divide(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Integer(left / right))),
            (left, right) => Err(Error::InvalidIntDiv(left.clone(), right.clone())),
        }
    }
}

#[derive(Debug, Clone)]
struct Scope {
    name: String,
    vars: HashMap<String, Object>,
    parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn with_parent(name: impl Into<String>, parent: Scope) -> Scope {
        Scope {
            name: name.into(),
            vars: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn parent(self) -> Option<Scope> {
        self.parent.map(|s| *s)
    }

    pub fn get(&mut self, name: impl AsRef<str>) -> Option<&Object> {
        let name = name.as_ref();
        if let Some(object) = self.vars.get(name) {
            return Some(object);
        };

        if let Some(ref mut scope) = self.parent {
            return scope.get(name);
        }
        None
    }

    pub fn set(&mut self, name: impl Into<String>, object: impl Into<Object>) {
        self.vars.insert(name.into(), object.into());
    }
}
