use crate::ast::*;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub enum Error {
    UnknownVariable(String),
    UnknownScope,
    UnknownFunction(String), // Procedure?

    InvalidArgument,

    CannotRead,

    InvalidAdd(Object, Object),
    InvalidSub(Object, Object),
    InvalidMul(Object, Object),
    InvalidIntDiv(Object, Object),

    InvalidUnaryAdd(Object),
    InvalidUnarySub(Object),
    InvalidUnaryNot(Object),

    InvalidAnd(Object, Object),
    InvalidOr(Object, Object),
    InvalidLessThan(Object, Object),
    InvalidGreaterThan(Object, Object),
    InvalidLessThanEqual(Object, Object),
    InvalidGreaterThanEqual(Object, Object),
    InvalidEqual(Object, Object),
    InvalidNotEqual(Object, Object),
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
                Declaration::Procedure(list) => {
                    for proc in list {
                        self.visit_procedure_declaration(proc)?;
                    }
                }
                Declaration::Function(list) => {
                    for func in list {
                        self.visit_function_declaration(func)?;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn visit_procedure_declaration(&mut self, node: &ProcedureDeclaration) -> Result<(), Error> {
        let (name, list, block) = (&node.0, &node.1, &node.2);
        let params = self.visit_formal_parameter_list(list)?;
        self.scope()?.set(
            name.clone(),
            Object::Procedure(name.clone(), params, block.clone()),
        );
        Ok(())
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclaration) -> Result<(), Error> {
        let (name, list, block, ty) = (&node.0, &node.1, &node.2, &node.3);
        let params = self.visit_formal_parameter_list(list)?;
        self.scope()?.set(
            name.clone(),
            Object::Function(name.clone(), params, block.clone(), ty.clone()),
        );
        Ok(())
    }

    fn visit_formal_parameter_list(
        &mut self,
        node: &FormalParameterList,
    ) -> Result<Vec<String>, Error> {
        let params = &node.0;
        let mut names = vec![];

        for param in params {
            let mut new = self.visit_formal_parameter(param)?;
            names.append(&mut new);
        }

        Ok(names)
    }

    fn visit_formal_parameter(&mut self, node: &FormalParameter) -> Result<Vec<String>, Error> {
        Ok(node.0.to_vec())
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
            Statement::FunctionCall(call) => self.visit_function_call(call),
            Statement::IfStatement(stmt) => self.visit_if_statement(stmt),
        }
    }

    fn visit_function_call(&mut self, node: &FunctionCall) -> Result<Object, Error> {
        let FunctionCall(Variable(func), CallParams(params)) = node;
        match self.scope()?.get(func).cloned() {
            Some(Object::Function(name, args, block, _))
            | Some(Object::Procedure(name, args, block)) => {
                self.enter(name.clone());
                for (a, p) in args.iter().zip(params.iter()) {
                    let p = self.visit_expression(p)?;
                    self.scope()?.set(a.to_string(), p);
                }
                let result = self.visit_block(&block)?;
                self.leave();
                Ok(result)
            }

            Some(Object::Builtin(Builtin::Write(f)))
            | Some(Object::Builtin(Builtin::WriteLn(f))) => {
                match self.visit_expression(&params[0])? {
                    o @ Object::Primitive(Primitive::Integer(_))
                    | o @ Object::Primitive(Primitive::String(_))
                    | o @ Object::Primitive(Primitive::Boolean(_)) => f(o),

                    o => {
                        warn!("invalid argument: {:#?}", o);
                        Err(Error::InvalidArgument)
                    }
                }
            }

            Some(Object::Builtin(Builtin::ReadLn(f))) => f(),

            _ => Err(Error::UnknownFunction(func.to_string()))?,
        }
    }

    fn visit_if_statement(&mut self, node: &IfStatement) -> Result<Object, Error> {
        unimplemented!()
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
            Expression::FunctionCall(call) => self.visit_function_call(call),
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
        use crate::ast::{BinaryExpression as Ex, BinaryOperator as Op};
        let mut visit = |e| self.visit_expression(e);
        match node {
            Ex(left, Op::Plus, expr) => visit(left)?.add(&visit(expr)?),
            Ex(left, Op::Minus, expr) => visit(left)?.subtract(&visit(expr)?),
            Ex(left, Op::Mul, expr) => visit(left)?.multiply(&visit(expr)?),
            Ex(left, Op::Div, expr) => visit(left)?.int_divide(&visit(expr)?),

            Ex(left, Op::And, expr) => visit(left)?.and(&visit(expr)?),
            Ex(left, Op::Or, expr) => visit(left)?.or(&visit(expr)?),
            Ex(left, Op::LessThan, expr) => visit(left)?.less_than(&visit(expr)?),
            Ex(left, Op::GreaterThan, expr) => visit(left)?.greater_than(&visit(expr)?),
            Ex(left, Op::LessThanEqual, expr) => visit(left)?.less_than_equal(&visit(expr)?),
            Ex(left, Op::GreaterThanEqual, expr) => visit(left)?.greater_than_equal(&visit(expr)?),
            Ex(left, Op::Equal, expr) => visit(left)?.equal(&visit(expr)?),
            Ex(left, Op::NotEqual, expr) => visit(left)?.not_equal(&visit(expr)?),
        }
    }

    fn visit_literal(&mut self, node: &Literal) -> Result<Object, Error> {
        Ok(match node {
            Literal::Integer(n) => Object::Primitive(Primitive::Integer(*n)),
            Literal::String(s) => Object::Primitive(Primitive::String(s.clone())),
            Literal::Boolean(b) => Object::Primitive(Primitive::Boolean(*b)),
        })
    }

    fn visit_variable(&mut self, node: &Variable) -> Result<Object, Error> {
        match self.scope()?.get(&node.0) {
            Some(object) => Ok(object.clone()),
            None => Err(Error::UnknownVariable(node.0.clone())),
        }
    }

    fn init(&mut self) -> Result<(), Error> {
        let scope = self.scope()?;
        scope.set("write", Object::Builtin(Builtin::Write(builtin::write)));
        scope.set(
            "writeln",
            Object::Builtin(Builtin::WriteLn(builtin::writeln)),
        );
        scope.set("readln", Object::Builtin(Builtin::ReadLn(builtin::readln)));
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
pub enum Object {
    Unit,
    Primitive(Primitive),
    Procedure(String, Vec<String>, crate::ast::Block),
    Function(String, Vec<String>, crate::ast::Block, crate::ast::Type),
    Builtin(Builtin),
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Integer(i32),
    String(String),
    Boolean(bool),
}
#[derive(Debug, Clone)]
pub enum Builtin {
    Write(fn(Object) -> Result<Object, Error>),
    WriteLn(fn(Object) -> Result<Object, Error>),
    ReadLn(fn() -> Result<Object, Error>),
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

    pub fn and(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Boolean(left)),
                Object::Primitive(Primitive::Boolean(right)),
            ) => Ok(Object::Primitive(Primitive::Boolean(*left && *right))),
            (left, right) => Err(Error::InvalidAnd(left.clone(), right.clone())),
        }
    }
    pub fn or(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Boolean(left)),
                Object::Primitive(Primitive::Boolean(right)),
            ) => Ok(Object::Primitive(Primitive::Boolean(*left || *right))),
            (left, right) => Err(Error::InvalidOr(left.clone(), right.clone())),
        }
    }
    pub fn less_than(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Boolean(left < right))),

            (left, right) => Err(Error::InvalidLessThan(left.clone(), right.clone())),
        }
    }
    pub fn greater_than(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Boolean(left > right))),

            (left, right) => Err(Error::InvalidGreaterThan(left.clone(), right.clone())),
        }
    }
    pub fn less_than_equal(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Boolean(left <= right))),

            (left, right) => Err(Error::InvalidLessThanEqual(left.clone(), right.clone())),
        }
    }
    pub fn greater_than_equal(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Boolean(left)),
                Object::Primitive(Primitive::Boolean(right)),
            ) => Ok(Object::Primitive(Primitive::Boolean(left >= right))),

            (left, right) => Err(Error::InvalidGreaterThanEqual(left.clone(), right.clone())),
        }
    }
    pub fn equal(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Boolean(left == right))),

            (left, right) => Err(Error::InvalidEqual(left.clone(), right.clone())),
        }
    }
    pub fn not_equal(&self, other: &Self) -> Result<Self, Error> {
        match (self, other) {
            (
                Object::Primitive(Primitive::Integer(left)),
                Object::Primitive(Primitive::Integer(right)),
            ) => Ok(Object::Primitive(Primitive::Boolean(left != right))),

            (left, right) => Err(Error::InvalidNotEqual(left.clone(), right.clone())),
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

pub(crate) mod builtin {
    use super::*;
    // builtins
    // TODO use a io::Read and io::Write as a backend
    pub(crate) fn write(data: Object) -> Result<Object, Error> {
        use std::io::prelude::*;
        use std::io::stdout;

        match data {
            Object::Primitive(Primitive::Integer(n)) => print!("{}", n),
            Object::Primitive(Primitive::String(s)) => print!("{}", s),
            Object::Primitive(Primitive::Boolean(b)) => print!("{}", b),
            _ => return Err(Error::InvalidArgument),
        }

        stdout().flush().expect("flush");
        Ok(Object::Unit)
    }

    pub(crate) fn writeln(data: Object) -> Result<Object, Error> {
        use std::io::prelude::*;
        use std::io::stdout;

        match data {
            Object::Primitive(Primitive::Integer(n)) => println!("{}", n),
            Object::Primitive(Primitive::String(s)) => println!("{}", s),
            Object::Primitive(Primitive::Boolean(b)) => println!("{}", b),
            _ => return Err(Error::InvalidArgument),
        }

        stdout().flush().expect("flush");
        Ok(Object::Unit)
    }

    pub(crate) fn readln() -> Result<Object, Error> {
        use std::io::prelude::*;
        use std::io::stdin;

        let mut buf = String::new();
        stdin()
            .read_line(&mut buf)
            .map_err(|e| {
                debug!("cannot read stdin: {}", e);
                Error::CannotRead
            })
            .and_then(|_| Ok(Object::Primitive(Primitive::String(buf))))
    }
}
