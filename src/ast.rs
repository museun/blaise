// TODO carry the span into the AST
#[derive(Debug)]
pub struct Program(pub Variable, pub Block);

#[derive(Debug)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    // if
    // call
}

#[derive(Debug)]
pub struct Assignment(pub Variable, pub Expression);

#[derive(Debug)]
pub enum Expression {
    Unary(Box<UnaryExpression>),
    Binary(Box<BinaryExpression>),
    Literal(Literal),
    Variable(Variable),
    // call
    // group (...)
}

#[derive(Debug)]
pub struct UnaryExpression(pub UnaryOperator, pub Expression);

#[derive(Debug)]
pub struct BinaryExpression(pub Expression, pub BinaryOperator, pub Expression);

#[derive(Debug)]
pub enum Literal {
    Integer(i32),
    String(String),
    // Bool(bool),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    Div,
    // / (float) (they are called reals)
    // and
    // or
    // <
    // <=
    // >
    // >=
    // =
    // <>
    // mod ?
}

#[derive(Debug)]
pub struct Block(pub Vec<Declaration>, pub Compound);

#[derive(Debug)]
pub struct Compound(pub Vec<Statement>);

#[derive(Debug)]
pub enum Declaration {
    // Procedure(Vec<ProcDeclaration>),
    // Function(Vec<FunctionDeclaration>),
    Variable(Vec<VariableDeclaration>),
    Empty,
}

#[derive(Debug)]
pub struct VariableDeclaration(pub Vec<String>, pub Type);

#[derive(Debug)]
pub struct Variable(pub String);

#[derive(Debug)]
pub enum Type {
    Integer,
    // Real,
    String,
    // Boolean,
    Unit,
}
