use crate::ast::*;
use crate::span::Span;
use crate::tokens::*;

pub struct Parser<'a> {
    tokens: Tokens<'a>,
}

pub struct Error<'a> {
    kind: ErrorKind,
    span: Span<'a>,
}

impl<'a> Error<'a> {
    pub fn new(kind: impl Into<ErrorKind>, span: Span<'a>) -> Self {
        Self {
            kind: kind.into(),
            span,
        }
    }

    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    pub fn span(&self) -> &Span<'a> {
        &self.span
    }
}

pub enum ErrorKind {
    UnexpectedEof,
    Unknown(String),
    Expected(Token),
}

impl From<Token> for ErrorKind {
    fn from(token: Token) -> Self {
        ErrorKind::Expected(token)
    }
}

// bit of handwaving to convert a string into a token
// it only handles Symbols and Reserved Keywords
// or into a custom error message
impl<'a> From<&'a str> for ErrorKind {
    fn from(s: &'a str) -> Self {
        if let Some(t) = Token::try_parse(s) {
            return t.into();
        }
        ErrorKind::Unknown(s.into())
    }
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: Tokens<'a>) -> Result<Program, Error> {
        let mut this = Self { tokens };
        let program = this.program()?;
        match this.tokens.next().unwrap() {
            Token::EOF => Ok(program),
            t => this.error(Token::EOF),
        }
    }

    fn error<T>(&self, err: impl Into<ErrorKind>) -> Result<T, Error> {
        let span = self.tokens.span().clone()
        Err(Error::new(err.into(),span))
    }

    fn program(&mut self) -> Result<Program, Error> {
        if !self.consume(Token::Reserved(Reserved::Program)) {
            return self.error("program");
        }

        let variable = self.variable();
        if !self.consume(Token::Symbol(Symbol::SemiColon)) {
            return self.error(";");
        }

        let block = self.block();
        if !self.consume(Token::Symbol(Symbol::Period)) {
            return self.error(".");
        }

        Ok(Program(variable, block))
    }

    fn block(&mut self) -> Block {
        let decls = self.declarations();
        let compound = self.compound_statement();
        Block(decls, compound)
    }

    fn declarations(&mut self) -> Vec<Declaration> {
        let mut decls = vec![];
        if let Token::Reserved(Reserved::Var) = self.tokens.peek().unwrap() {
            self.tokens.advance();
            let mut vars = vec![];
            while let Token::Identifier(_) = self.tokens.peek().unwrap() {
                vars.push(self.variable_declaration())
            }
            if !vars.is_empty() {
                decls.push(Declaration::Variable(vars))
            } else {
                panic!("expected at least one variable decl after var");
            }
        }

        let mut procs = vec![];
        let mut funcs = vec![];

        loop {
            match self.tokens.peek().unwrap() {
                Token::Reserved(Reserved::Procedure) => procs.push(self.procedure_declaration()),
                Token::Reserved(Reserved::Function) => funcs.push(self.function_declaration()),
                _ => break,
            };
        }

        if !procs.is_empty() {
            decls.push(Declaration::Procedure(procs));
        }

        if !funcs.is_empty() {
            decls.push(Declaration::Function(funcs));
        }

        if decls.is_empty() {
            decls.push(Declaration::Empty)
        }
        decls
    }

    fn procedure_declaration(&mut self) -> ProcedureDeclaration {
        // proc decl ::= PROC ID (LPAREN, list, RPAREN)? SEMI block
        let name = match (self.tokens.next().unwrap(), self.tokens.next().unwrap()) {
            (Token::Reserved(Reserved::Procedure), Token::Identifier(name)) => name,
            (l, r) => panic!("{:?} | {:?}", l, r),
        };

        let params = match self.tokens.peek().unwrap() {
            Token::Symbol(Symbol::OpenParen) => {
                self.tokens.advance();
                match (self.formal_parameter_list(), self.tokens.next().unwrap()) {
                    (list, Token::Symbol(Symbol::CloseParen)) => list,
                    (_, t) => panic!("{:#?}", t),
                }
            }
            _ => FormalParameterList(vec![]),
        };

        let block = match self.tokens.next().unwrap() {
            Token::Symbol(Symbol::SemiColon) => self.block(),
            t => panic!("{:#?}", t),
        };

        ProcedureDeclaration(name, params, block)
    }

    fn function_declaration(&mut self) -> FunctionDeclaration {
        // func decl ::= FUNC ID LPAREN list RPAREN COLON type SEMI block
        let name = match (self.tokens.next().unwrap(), self.tokens.next().unwrap()) {
            (Token::Reserved(Reserved::Function), Token::Identifier(name)) => name,
            (l, r) => panic!("{:?} | {:?}", l, r),
        };

        let params = match self.tokens.next().unwrap() {
            Token::Symbol(Symbol::OpenParen) => {
                match (self.formal_parameter_list(), self.tokens.next().unwrap()) {
                    (list, Token::Symbol(Symbol::CloseParen)) => list,
                    (_, t) => panic!("{:#?}", t),
                }
            }
            _ => FormalParameterList(vec![]),
        };

        let ret = match self.tokens.next().unwrap() {
            Token::Symbol(Symbol::Colon) => self.ty(),
            t => panic!("{:#?}", t),
        };

        let block = match self.tokens.next().unwrap() {
            Token::Symbol(Symbol::SemiColon) => self.block(),
            t => panic!("{:#?}", t),
        };

        FunctionDeclaration(name, params, block, ret)
    }

    fn variable_declaration(&mut self) -> VariableDeclaration {
        let mut idents = vec![];
        match self.tokens.next().unwrap() {
            Token::Identifier(id) => idents.push(id),
            t => panic!("{:#?}", t),
        };

        while let Token::Symbol(Symbol::Comma) = self.tokens.peek().unwrap() {
            self.tokens.advance(); // eat comma
            match self.tokens.next().unwrap() {
                Token::Identifier(id) => idents.push(id),
                t => panic!("{:#?}", t),
            };
        }

        // var a,b,c,d : integer;
        match self.tokens.next().unwrap() {
            Token::Symbol(Symbol::Colon) => match (self.ty(), self.tokens.next().unwrap()) {
                (ty, Token::Symbol(Symbol::SemiColon)) => VariableDeclaration(idents, ty),
                t => panic!("expected ; after {:#?}", t),
            },
            t => panic!("{:#?}", t),
        }
    }

    fn compound_statement(&mut self) -> Compound {
        match self.tokens.next().unwrap() {
            Token::Reserved(Reserved::Begin) => {}
            token => panic!("{:#?}", token),
        };

        let mut statements = vec![];
        while !self.consume(Token::Reserved(Reserved::End)) {
            statements.push(self.statement())
        }
        Compound(statements)
    }

    fn statement(&mut self) -> Statement {
        match self.tokens.peek().unwrap() {
            Token::Reserved(Reserved::Begin) => Statement::Compound(self.compound_statement()),
            Token::Identifier(_) => match self.tokens.peek_ahead(1).unwrap() {
                Token::Symbol(Symbol::OpenParen) => Statement::FunctionCall(self.function_call()),
                Token::Symbol(Symbol::Assign) => Statement::Assignment(self.assignment_statement()),
                t => panic!("{:#?}", t),
            },
            Token::Reserved(Reserved::If) => Statement::IfStatement(self.if_statement()),
            t => panic!("{:#?}", t),
        }
    }

    fn function_call(&mut self) -> FunctionCall {
        let id = match (self.variable(), self.tokens.next().unwrap()) {
            (var, Token::Symbol(Symbol::OpenParen)) => var,
            (var, t) => panic!("{:#?} var: {:#?}", var, t),
        };

        match (
            self.call_params(),
            self.tokens.next().unwrap(),
            self.tokens.next().unwrap(),
        ) {
            (params, Token::Symbol(Symbol::CloseParen), Token::Symbol(Symbol::SemiColon)) => {
                FunctionCall(id, params)
            }
            (params, l, r) => panic!("{:#?} params {:#?} | {:#?}", params, l, r),
        }
    }

    fn function_call_expr(&mut self, left: &Expression) -> FunctionCall {
        match (left, self.tokens.current().unwrap()) {
            (Expression::Variable(name), Token::Symbol(Symbol::OpenParen)) => {
                match (self.call_params(), self.tokens.next().unwrap()) {
                    (params, Token::Symbol(Symbol::CloseParen)) => {
                        FunctionCall(name.clone(), params)
                    }
                    (params, ty) => panic!("{:#?} | {:#?}", params, ty),
                }
            }
            t => panic!("{:#?}", t),
        }
    }

    fn if_statement(&mut self) -> IfStatement {
        // if_stmt ::= IF expr THEN comp_stmt (ELSE (if_stmt | comp_stmt))?
        let (expr, compound) = match self.tokens.next().unwrap() {
            Token::Reserved(Reserved::If) => {
                match (self.expression(None), self.tokens.next().unwrap()) {
                    (expr, Token::Reserved(Reserved::Then)) => {
                        (expr, { self.compound_statement() })
                    }
                    (expr, t) => panic!("{:#?} expr {:#?}", expr, t),
                }
            }
            t => panic!("{:#?}", t),
        };

        match self.tokens.peek().unwrap() {
            Token::Reserved(Reserved::Else) => {
                self.tokens.advance();
                match self.tokens.peek().unwrap() {
                    Token::Reserved(Reserved::If) => {
                        IfStatement::IfElseIf(expr, compound, Box::new(self.if_statement()))
                    }
                    _ => IfStatement::IfElse(expr, compound, self.compound_statement()),
                }
            }
            _ => IfStatement::If(expr, compound),
        }
    }

    fn call_params(&mut self) -> CallParams {
        if let Token::Symbol(Symbol::CloseParen) = self.tokens.peek().unwrap() {
            CallParams(vec![])
        } else {
            let mut params = vec![self.expression(None)];
            while let Token::Symbol(Symbol::Comma) = self.tokens.peek().unwrap() {
                self.tokens.next();
                params.push(self.expression(None))
            }
            CallParams(params)
        }
    }

    fn formal_parameter_list(&mut self) -> FormalParameterList {
        let mut list = vec![];
        if let Token::Identifier(_) = self.tokens.peek().unwrap() {
            loop {
                list.push(self.formal_parameter());
                match self.tokens.peek().unwrap() {
                    Token::Symbol(Symbol::SemiColon) => self.tokens.advance(),
                    _ => break,
                };
            }
        }

        FormalParameterList(list)
    }

    fn formal_parameter(&mut self) -> FormalParameter {
        let mut idents = vec![];

        match self.tokens.next().unwrap() {
            Token::Identifier(id) => idents.push(id),
            t => panic!("{:#?}", t),
        };

        while let Token::Symbol(Symbol::Comma) = self.tokens.peek().unwrap() {
            self.tokens.next();
            match self.tokens.next().unwrap() {
                Token::Identifier(id) => idents.push(id),
                t => panic!("{:#?}", t),
            }
        }

        let ty = match self.tokens.next().unwrap() {
            Token::Symbol(Symbol::Colon) => self.ty(),
            t => panic!("{:#?}", t),
        };

        FormalParameter(idents, ty)
    }

    fn assignment_statement(&mut self) -> Assignment {
        match (self.variable(), self.tokens.next().unwrap()) {
            (var, Token::Symbol(Symbol::Assign)) => {
                match (self.expression(None), self.tokens.next().unwrap()) {
                    (expr, Token::Symbol(Symbol::SemiColon)) => Assignment(var, expr),
                    t => panic!("expected ; after {:#?}", t),
                }
            }
            t => panic!("expected = after {:#?}", t),
        }
    }

    fn expression(&mut self, p: Option<u32>) -> Expression {
        let p = p.unwrap_or(0);

        let token = self.tokens.next().unwrap();

        let parser = self.prefix_parser(&token).expect("prefix parser");
        let mut lhs = parser.parse(self);

        while p < self.next_precedence() {
            let token = self.tokens.next().expect("next token");
            let parser = self.infix_parser(&token).expect("infix parser");
            lhs = parser.parse(self, lhs);
        }

        lhs
    }

    fn variable(&mut self) -> Variable {
        match self.tokens.next().unwrap() {
            Token::Identifier(id) => Variable(id),
            token => panic!("{:#?}", token),
        }
    }

    fn variable_expr(&mut self) -> Variable {
        match self.tokens.current().unwrap() {
            Token::Identifier(id) => Variable(id.clone()),
            token => panic!("{:#?}", token),
        }
    }

    fn unary_op(&mut self) -> UnaryExpression {
        let op = match self.tokens.current().unwrap() {
            Token::Symbol(Symbol::Plus) => UnaryOperator::Plus,
            Token::Symbol(Symbol::Minus) => UnaryOperator::Minus,
            Token::Reserved(Reserved::Not) => UnaryOperator::Not,
            t => panic!("{:#?}", t),
        };
        UnaryExpression(op, self.expression(None))
    }

    fn binary_op(&mut self, expr: Expression) -> BinaryExpression {
        let op = match self.tokens.current().unwrap() {
            Token::Symbol(Symbol::Plus) => BinaryOperator::Plus,
            Token::Symbol(Symbol::Minus) => BinaryOperator::Minus,
            Token::Symbol(Symbol::Mul) => BinaryOperator::Mul,
            Token::Symbol(Symbol::Div) => BinaryOperator::Div,

            Token::Reserved(Reserved::And) => BinaryOperator::And,
            Token::Reserved(Reserved::Or) => BinaryOperator::Or,
            Token::Symbol(Symbol::LessThan) => BinaryOperator::LessThan,
            Token::Symbol(Symbol::GreaterThan) => BinaryOperator::GreaterThan,
            Token::Symbol(Symbol::LessThanEqual) => BinaryOperator::LessThanEqual,
            Token::Symbol(Symbol::GreaterThanEqual) => BinaryOperator::GreaterThanEqual,
            Token::Symbol(Symbol::Equal) => BinaryOperator::Equal,
            Token::Symbol(Symbol::NotEqual) => BinaryOperator::NotEqual,

            t => panic!("{:#?}", t),
        };
        BinaryExpression(expr, op, self.expression(None))
    }

    fn literal(&mut self) -> Literal {
        let t = self.tokens.current().unwrap();
        match t {
            Token::Number(n) => Literal::Integer(*n),
            Token::String(s) => Literal::String(s.clone()),
            t => panic!("{:#?}", t),
        }
    }

    fn ty(&mut self) -> crate::ast::Type {
        match self.tokens.next().unwrap() {
            Token::Type(crate::tokens::Type::Integer) => crate::ast::Type::Integer,
            Token::Type(crate::tokens::Type::String) => crate::ast::Type::String,
            t => panic!("{:#?}", t),
        }
    }

    fn consume(&mut self, tok: impl Into<Token>) -> bool {
        let tok = tok.into();

        match self.tokens.peek().unwrap() {
            t if *t == tok => {
                self.tokens.advance();
                true
            }
            _ => false,
        }
    }

    fn prefix_parser(&mut self, token: &Token) -> Option<PrefixParser> {
        match token {
            Token::Number(_) | Token::String(_) => Some(PrefixParser::Literal),
            Token::Symbol(Symbol::Plus) | Token::Symbol(Symbol::Minus) => Some(
                PrefixParser::UnaryOperator(Precendence::UnaryLiteral as u32),
            ),
            Token::Reserved(Reserved::Not) => {
                Some(PrefixParser::UnaryOperator(Precendence::UnaryBool as u32))
            }
            Token::Identifier(_) => Some(PrefixParser::Variable),
            _ => None,
        }
    }

    fn infix_parser(&mut self, token: &Token) -> Option<InfixParser> {
        match token {
            Token::Symbol(Symbol::Plus) | Token::Symbol(Symbol::Minus) => {
                Some(InfixParser::BinaryOperator(Precendence::Addition as u32))
            }
            Token::Symbol(Symbol::Mul) | Token::Symbol(Symbol::Div) => Some(
                InfixParser::BinaryOperator(Precendence::Multiplication as u32),
            ),
            Token::Reserved(Reserved::And) | Token::Reserved(Reserved::Or) => {
                Some(InfixParser::BinaryOperator(Precendence::BinaryBool as u32))
            }
            Token::Symbol(Symbol::LessThan)
            | Token::Symbol(Symbol::GreaterThan)
            | Token::Symbol(Symbol::LessThanEqual)
            | Token::Symbol(Symbol::GreaterThanEqual)
            | Token::Symbol(Symbol::Equal)
            | Token::Symbol(Symbol::NotEqual) => {
                Some(InfixParser::BinaryOperator(Precendence::Relative as u32))
            }
            _ => None,
        }
    }

    fn next_precedence(&mut self) -> u32 {
        let token = self.tokens.peek().unwrap();
        let token = token.clone();
        match self.infix_parser(&token) {
            Some(pp) => pp.precedence(),
            _ => 0,
        }
    }
}

// Operator precedences shall be according to four classes of operators
// as follows. The operator not shall have the highest precedence,
// followed by the multiplying-operators, then the adding-operators and
// signs, and finally, with the lowest precedence, the relational-
// operators. Sequences of two or more operators of the same precedence
// shall be left associative.

enum PrefixParser {
    Literal,
    Variable,
    UnaryOperator(u32),
    // grouping (...)
}

enum InfixParser {
    BinaryOperator(u32),
    FunctionCall(u32),
}

impl PrefixParser {
    pub fn parse(&self, parser: &mut Parser) -> Expression {
        match self {
            PrefixParser::Literal => Expression::Literal(parser.literal()),
            PrefixParser::Variable => Expression::Variable(parser.variable_expr()),
            PrefixParser::UnaryOperator(_) => Expression::Unary(Box::new(parser.unary_op())),
        }
    }

    pub fn precedence(&self) -> u32 {
        match self {
            PrefixParser::UnaryOperator(p) => *p,
            _ => 0,
        }
    }
}

impl InfixParser {
    pub fn parse(&self, parser: &mut Parser, left: Expression) -> Expression {
        match self {
            InfixParser::BinaryOperator(_) => Expression::Binary(Box::new(parser.binary_op(left))),
            InfixParser::FunctionCall(_) => {
                Expression::FunctionCall(parser.function_call_expr(&left))
            }
        }
    }

    pub fn precedence(&self) -> u32 {
        match self {
            InfixParser::BinaryOperator(p) | InfixParser::FunctionCall(p) => *p,
        }
    }
}

enum Precendence {
    Call = 7,
    UnaryLiteral = 6,
    Multiplication = 5,
    Addition = 4,
    Relative = 3,
    UnaryBool = 2,
    BinaryBool = 1,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::*;

    // I want the AST to be an ADT
    fn make_ast(input: &str) -> Program {
        let mut tokens = Lexer::scan("stdin", &input);
        tokens.remove_comments();

        // this shouldn't return a Program, but some Fragment
        Parser::parse(tokens)
    }

    macro_rules! check {
        ($l:expr, $($p:pat)|*) => {
            // use std::mem::discriminant;
            // let l = discriminant(&$l);
            // let r = discriminant(&$r);
            match $l {
                $($p)|* => (),
                ref result => panic!("expected: {}, got: {:?}", stringify!($($p)|*), result),
            }
        };
    }

    #[test]
    fn program() {
        let ast = make_ast(
            r#"
program test;
"#,
        );

        check!(ast, Program(_, _));
    }
}
