use crate::prelude::*;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Program(pub Variable, pub Block);

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Declaration>, pub Compound);
impl Default for Block {
    fn default() -> Self {
        Self {
            0: vec![
                Declaration::Variable(vec![]),
                Declaration::Procedure(vec![]),
                Declaration::Function(vec![]),
            ],
            1: Compound::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Variable(Vec<VariableDeclaration>),
    Procedure(Vec<ProcedureDeclaration>),
    Function(Vec<FunctionDeclaration>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Compound(pub Vec<Statement>);
impl Default for Compound {
    fn default() -> Self {
        Self {
            0: vec![Statement::Empty],
        }
    }
}

// case
// with
// goto

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    FunctionCall(FunctionCall),
    IfStatement(Box<IfStatement>),
    Repetitive(Repetitive),
    Empty,
}

impl Default for Statement {
    fn default() -> Self {
        Statement::Empty
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Repetitive {
    Repeat(Vec<Statement>, Expression),
    While(Expression, Compound),
    For(Variable, Expression, Direction, Expression, Compound),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Direction {
    To,
    DownTo,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfStatement {
    If(Expression, Statement),
    IfElse(Expression, Statement, Statement),
    IfElseIf(Expression, Statement, Box<IfStatement>),
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct FunctionCall(pub Variable, pub CallParams);

#[derive(Default, Debug, Clone, PartialEq)]
pub struct CallParams(pub Vec<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment(pub Variable, pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Unary(Box<UnaryExpression>),
    Binary(Box<BinaryExpression>),
    Boolean(Box<BinaryExpression>),
    Literal(Literal),
    Variable(Variable),
    FunctionCall(FunctionCall),
    Group(Box<GroupExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression(pub UnaryOperator, pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression(pub Expression, pub BinaryOperator, pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub struct GroupExpression(pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    String(String),
    Boolean(bool),
    Real(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    IntDiv,
    Div,

    // bool
    And,
    Or,

    // relational
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equal,
    NotEqual,
}

use std::fmt;
impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperator::Plus => "+",
                BinaryOperator::Minus => "-",
                BinaryOperator::Mul => "*",
                BinaryOperator::IntDiv => "div",
                BinaryOperator::Div => "/",
                BinaryOperator::And => "and",
                BinaryOperator::Or => "or",
                BinaryOperator::LessThan => "<",
                BinaryOperator::GreaterThan => ">",
                BinaryOperator::LessThanEqual => "<=",
                BinaryOperator::GreaterThanEqual => ">=",
                BinaryOperator::Equal => "=",
                BinaryOperator::NotEqual => "<>",
            }
        )
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct ProcedureDeclaration(pub String, pub FormalParameterList, pub Block);

#[derive(Default, Debug, Clone, PartialEq)]
pub struct FunctionDeclaration(pub String, pub FormalParameterList, pub Block, pub Type);

#[derive(Default, Debug, Clone, PartialEq)]
pub struct FormalParameterList(pub Vec<FormalParameter>);

#[derive(Default, Debug, Clone, PartialEq)]
pub struct FormalParameter(pub Vec<String>, pub Type);

#[derive(Default, Debug, Clone, PartialEq)]
pub struct VariableDeclaration(pub Vec<String>, pub Type);

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Variable(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    String,
    Boolean,
    Real,
    Unit,
}

impl Default for Type {
    fn default() -> Self {
        Type::Unit
    }
}

impl From<token::Type> for Type {
    fn from(ty: token::Type) -> Self {
        match ty {
            token::Type::Integer => Type::Integer,
            token::Type::String => Type::String,
            token::Type::Boolean => Type::Boolean,
            token::Type::Real => Type::Real,
            _ => panic!("unit type isn't a real type"),
        }
    }
}
