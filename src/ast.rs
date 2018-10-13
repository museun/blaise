// TODO carry the span into the AST
#[derive(Debug, Clone, PartialEq)]
pub struct Program(pub Variable, pub Block);

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Declaration>, pub Compound);

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Procedure(Vec<ProcedureDeclaration>),
    Function(Vec<FunctionDeclaration>),
    Variable(Vec<VariableDeclaration>),
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Compound(pub Vec<Statement>);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    FunctionCall(FunctionCall),
    IfStatement(IfStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfStatement {
    If(Expression, Compound),
    IfElse(Expression, Compound, Compound),
    IfElseIf(Expression, Compound, Box<IfStatement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall(pub Variable, pub CallParams);

#[derive(Debug, Clone, PartialEq)]
pub struct CallParams(pub Vec<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment(pub Variable, pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Unary(Box<UnaryExpression>),
    Binary(Box<BinaryExpression>),
    Literal(Literal),
    Variable(Variable),
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression(pub UnaryOperator, pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression(pub Expression, pub BinaryOperator, pub Expression);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    String(String),
    // Bool(bool),
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
    Div,

    And,
    Or,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureDeclaration(pub String, pub FormalParameterList, pub Block);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration(pub String, pub FormalParameterList, pub Block, pub Type);

#[derive(Debug, Clone, PartialEq)]
pub struct FormalParameterList(pub Vec<FormalParameter>);

#[derive(Debug, Clone, PartialEq)]
pub struct FormalParameter(pub Vec<String>, pub Type);

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration(pub Vec<String>, pub Type);

#[derive(Debug, Clone, PartialEq)]
pub struct Variable(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    // Real,
    String,
    // Boolean,
    Unit,
}
