use crate::prelude::*;

mod error;
mod infix;
mod prefix;

use self::ast::*;
use self::infix::*;
use self::prefix::*;

pub mod ast;
pub use self::error::{Error, ErrorKind};

type Result<T> = ::std::result::Result<T, Error>;

pub struct Parser {
    tokens: Tokens,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Self {
        Self { tokens }
    }

    pub fn parse(mut self) -> Result<Program> {
        traced!("parsed");

        let program = self.program()?;
        let mut tokens = self.tokens;
        match tokens.next() {
            Some(Token::EOF) => Ok(program),
            // unexpected token
            Some(t) => Err(Error {
                kind: ErrorKind::Unexpected(t),
                span: tokens.span().clone(),
                source: tokens.source().into(),
            }),
            // EOF already seen
            None => Err(Error::new(
                Token::EOF,
                tokens.span().clone(),
                tokens.source(),
            )),
        }
    }

    pub fn program(&mut self) -> Result<Program> {
        traced!("program");

        self.expect_token("program")?;
        let variable = self.expect(Self::variable, ";")?;
        let block = self.expect(Self::block, ".")?;
        Ok(Program(variable, block))
    }

    pub fn block(&mut self) -> Result<Block> {
        traced!("block");

        let decls = self.declarations()?;
        trace!("decls: {:#?}", decls);
        let compound = self.compound_statement()?;
        Ok(Block(decls, compound))
    }

    fn declarations(&mut self) -> Result<Vec<Declaration>> {
        traced!("declarations");
        let mut decls = vec![];
        if let Token::Reserved(token::Reserved::Var) = self.tokens.peek() {
            self.tokens.advance();

            let mut vars = vec![];
            while let Token::Identifier(_) = self.tokens.peek() {
                vars.push(self.variable_declaration()?)
            }
            decls.push(Declaration::Variable(vars));
            if decls.is_empty() {
                self.error("expected at least one variable decl after var")?
            }
        }

        let (mut procs, mut funcs) = (vec![], vec![]);
        loop {
            match self.tokens.peek() {
                Token::Reserved(Reserved::Procedure) => {
                    procs.push(self.procedure_declaration()?);
                }
                Token::Reserved(Reserved::Function) => {
                    funcs.push(self.function_declaration()?);
                }
                _ => break,
            };
        }

        decls.push(Declaration::Procedure(procs));
        decls.push(Declaration::Function(funcs));

        Ok(decls)
    }

    fn procedure_declaration(&mut self) -> Result<ProcedureDeclaration> {
        traced!("procedure_declaration");
        // proc decl ::= PROC ID (LPAREN, list, RPAREN)? SEMI block
        self.expect_token("procedure")?;
        // TODO proc decl error
        let name = self.identifier()?;

        let params = match self.tokens.peek() {
            Token::Symbol(Symbol::OpenParen) => {
                self.tokens.advance();
                self.expect(Self::formal_parameter_list, ")")?
            }
            _ => FormalParameterList(vec![]),
        };

        self.expect_token(";")?;
        let block = self.expect(Self::block, ";")?;
        Ok(ProcedureDeclaration(name, params, block))
    }

    fn function_declaration(&mut self) -> Result<FunctionDeclaration> {
        traced!("function_declaration");
        // func decl ::= FUNC ID LPAREN list RPAREN COLON type SEMI block
        self.expect_token("function")?;
        // TODO func decl error
        let name = self.identifier()?;

        let params = match self.tokens.peek() {
            Token::Symbol(Symbol::OpenParen) => {
                self.tokens.advance();
                self.expect(Self::formal_parameter_list, ")")?
            }
            _ => FormalParameterList(vec![]),
        };

        self.expect_token(":")?;
        let ty = self.expect(Self::ty, ";")?;
        let block = self.expect(Self::block, ";")?;
        Ok(FunctionDeclaration(name, params, block, ty))
    }

    fn variable_declaration(&mut self) -> Result<VariableDeclaration> {
        traced!("variable_declaration");
        // var a,b,c,d : integer;
        // TODO identifier name
        let mut idents = vec![self.identifier()?];
        while self.consume(",") {
            idents.push(self.identifier()?);
        }

        self.expect_token(":")?;
        let ty = self.expect(Self::ty, ";")?;
        Ok(VariableDeclaration(idents, ty))
    }

    fn compound_statement(&mut self) -> Result<Compound> {
        traced!("compound_statement");
        // begin stmt1; stmt2; end
        self.expect_token("begin")?;
        let mut statements = vec![];
        while !self.consume("end") {
            statements.push(self.statement()?)
        }
        Ok(Compound(statements))
    }

    fn statement(&mut self) -> Result<Statement> {
        traced!("statement");
        use self::Statement::*;
        //use crate::prelude::token::Symbol;

        let res = match self.tokens.peek() {
            Token::Reserved(Reserved::Begin) => Compound(self.compound_statement()?),
            Token::Identifier(_) => match self.tokens.peek_ahead(1).unwrap() {
                Token::Symbol(Symbol::OpenParen) => FunctionCall(self.function_call()?),
                Token::Symbol(Symbol::Assign) => Assignment(self.assignment_statement()?),
                t => {
                    self.tokens.advance();
                    self.unexpected(t)?
                }
            },
            Token::Reserved(Reserved::If) => IfStatement(self.if_statement()?),
            t => self.unexpected(t)?,
        };

        Ok(res)
    }

    fn function_call(&mut self) -> Result<FunctionCall> {
        traced!("function_call");
        let id = self.expect(Self::variable, "(")?;
        let params = self.expect(Self::call_params, ")")?;
        self.expect_token(";")?;

        Ok(FunctionCall(id, params))
    }

    fn function_call_expr(&mut self, left: Expression) -> Result<FunctionCall> {
        traced!("function_call_expr");
        let name = match left {
            Expression::Variable(name) => name,
            _e => self.error("expression isn't a variable")?,
        };

        let params = if self.peek("(") {
            self.tokens.advance();
            self.call_params()?
        } else {
            CallParams(vec![])
        };
        Ok(FunctionCall(name.clone(), params))
    }

    fn if_statement(&mut self) -> Result<IfStatement> {
        traced!("if_statement");
        // if_stmt ::= IF expr THEN comp_stmt (ELSE (if_stmt | comp_stmt))?
        self.expect_token("if")?;
        let expr = self.expression(None)?;

        self.expect_token("then")?;
        let compound = self.compound_statement()?;

        let res = if self.consume("else") {
            if self.peek("if") {
                // don't eat the if
                IfStatement::IfElseIf(expr, compound, Box::new(self.if_statement()?))
            } else {
                IfStatement::IfElse(expr, compound, self.compound_statement()?)
            }
        } else {
            IfStatement::If(expr, compound)
        };
        Ok(res)
    }

    fn call_params(&mut self) -> Result<CallParams> {
        traced!("call_params");
        if self.peek(")") {
            self.tokens.advance();
            return Ok(CallParams(vec![]));
        }
        let mut params = vec![self.expression(None)?];
        while self.consume(",") {
            params.push(self.expression(None)?)
        }
        Ok(CallParams(params))
    }

    fn formal_parameter_list(&mut self) -> Result<FormalParameterList> {
        traced!("formal_parameter_list");
        let mut list = vec![];
        if let Token::Identifier(_) = self.tokens.peek() {
            list.push(self.formal_parameter()?);
            while self.consume(";") {
                list.push(self.formal_parameter()?);
            }
        }
        Ok(FormalParameterList(list))
    }

    fn formal_parameter(&mut self) -> Result<FormalParameter> {
        traced!("formal_parameter");
        let mut idents = vec![self.identifier()?];
        while self.consume(",") {
            idents.push(self.identifier()?)
        }

        self.expect_token(":")?;
        let ty = self.ty()?;
        Ok(FormalParameter(idents, ty))
    }

    fn assignment_statement(&mut self) -> Result<Assignment> {
        traced!("assignment_statement");
        let var = self.expect(Self::variable, ":=")?;
        let expr = self.expect(|p| Self::expression(p, None), ";")?;
        Ok(Assignment(var, expr))
    }

    fn expression(&mut self, p: Option<u32>) -> Result<Expression> {
        traced!("expression");
        let p = p.unwrap_or(0);
        let token = self.tokens.next_token();

        let parser = self
            .prefix_parser(&token)
            .ok_or_else(|| self.error::<(), _>(token).unwrap_err())?;

        let mut lhs = parser.parse(self)?;
        while p < self.next_precedence() {
            let token = self.tokens.next_token();
            let parser = self
                .infix_parser(&token)
                .ok_or_else(|| self.error::<(), _>(token).unwrap_err())?;
            lhs = parser.parse(self, lhs)?;
        }
        Ok(lhs)
    }

    fn variable(&mut self) -> Result<Variable> {
        traced!("variable");
        let id = self.identifier()?;
        Ok(Variable(id))
    }

    fn variable_expr(&mut self) -> Result<Variable> {
        traced!("variable_expr");
        match self.tokens.current() {
            Token::Identifier(id) => Ok(Variable(id.clone())),
            t => self.unexpected(t),
        }
    }

    fn unary_op(&mut self) -> Result<UnaryExpression> {
        traced!("unary_op");
        let op = match self.tokens.current() {
            Token::Symbol(Symbol::Plus) => UnaryOperator::Plus,
            Token::Symbol(Symbol::Minus) => UnaryOperator::Minus,
            Token::Reserved(Reserved::Not) => UnaryOperator::Not,
            t => self.unexpected(t)?,
        };
        let p = self.next_precedence();
        Ok(UnaryExpression(op, self.expression(Some(p))?))
    }

    fn literal(&mut self) -> Result<Literal> {
        traced!("literal");
        Ok(match self.tokens.current() {
            Token::Number(n) => Literal::Integer(n),
            Token::String(s) => Literal::String(s.clone()),
            t => self.unexpected(t)?,
        })
    }

    fn ty(&mut self) -> Result<Type> {
        traced!("ty");
        match self.tokens.next_token() {
            Token::Type(t) => Ok(t.into()),
            t => self.unexpected(t)?,
        }
    }

    fn prefix_parser(&mut self, token: &Token) -> Option<PrefixParser> {
        traced!("prefix_parser");
        match token {
            Token::Number(_) | Token::String(_) => Some(PrefixParser::Literal),
            Token::Symbol(Symbol::Plus) | Token::Symbol(Symbol::Minus) => {
                Some(PrefixParser::UnaryOperator(Precedence::UnaryLiteral))
            }
            Token::Reserved(Reserved::Not) => {
                Some(PrefixParser::UnaryOperator(Precedence::UnaryBool))
            }
            Token::Identifier(_) => Some(PrefixParser::Variable),
            _ => None,
        }
    }

    fn infix_parser(&mut self, token: &Token) -> Option<InfixParser> {
        traced!("infix_parser");
        use self::InfixParser::BinaryOperator as Op;
        use self::Precedence::*;
        use crate::prelude::token::{Reserved::*, Symbol::*};

        match token {
            Token::Symbol(Plus) | Token::Symbol(Minus) => Some(Op(BinaryAdd)),
            Token::Symbol(Symbol::Mul) => Some(Op(BinaryMul)),
            Token::Reserved(Reserved::Div) => Some(Op(BinaryMul)),
            Token::Reserved(And) | Token::Reserved(Or) => Some(Op(BinaryBool)),

            Token::Symbol(LessThan)
            | Token::Symbol(GreaterThan)
            | Token::Symbol(LessThanEqual)
            | Token::Symbol(GreaterThanEqual)
            | Token::Symbol(Equal)
            | Token::Symbol(NotEqual) => Some(Op(Relative)),

            Token::Symbol(OpenParen) => Some(InfixParser::FunctionCall(Call)),
            _ => None,
        }
    }

    fn identifier(&mut self) -> Result<String> {
        traced!("identifier");
        match self.tokens.next().ok_or_else(|| Error {
            kind: ErrorKind::Unexpected(Token::EOF),
            span: self.tokens.span().clone(),
            source: self.tokens.source().into(),
        })? {
            Token::Identifier(name) => Ok(name.clone()),
            t => self.unexpected(t)?,
        }
    }

    fn next_precedence(&mut self) -> u32 {
        traced!("next_precedence");
        let token = self.tokens.peek();
        match self.infix_parser(&token) {
            Some(pp) => pp.precedence(),
            _ => 0,
        }
    }

    fn consume(&mut self, tok: impl Into<Token>) -> bool {
        traced!("consume");
        let tok = tok.into();
        match self.tokens.peek() {
            ref t if *t == tok => {
                self.tokens.advance();
                true
            }
            _ => false,
        }
    }

    /// doesn't consume
    fn peek(&mut self, tok: impl Into<Token>) -> bool {
        traced!("peek");
        let tok = tok.into();
        match self.tokens.peek() {
            ref t if *t == tok => true,
            _ => false,
        }
    }

    fn expect<E, F>(&mut self, mut f: F, tok: impl Into<Token>) -> Result<E>
    where
        F: FnMut(&mut Parser) -> Result<E>,
    {
        let tok = tok.into();
        traced!("expect");
        let res = f(self).map_err(|e| {
            trace!("expected: {:?}", tok);
            e
        })?;

        self.expect_token(tok.clone()).map_err(|e| {
            trace!("expected: {:?}", tok);
            e
        })?;

        Ok(res)
    }

    // maybe this should peek
    fn expect_token(&mut self, tok: impl Into<Token>) -> Result<Token> {
        traced!("expect_token");
        let tok = tok.into();
        trace!("expecting: {:?}", tok);
        match self.tokens.peek() {
            ref t if *t == tok => {
                self.tokens.advance();
                Ok(tok)
            }
            Token::EOF => self.unexpected(Token::EOF)?,
            _ => self.expected(tok),
        }
    }

    // just to give it a better name
    fn expected<T>(&self, err: impl Into<ErrorKind>) -> Result<T> {
        traced!("expected");
        self.error(err)
    }

    fn unexpected<T, E>(&self, err: E) -> Result<T>
    where
        E: Into<Token> + Clone,
    {
        traced!("unexpected");
        let span = self.tokens.span().clone();
        let err = err.into().clone();
        debug!("{}: unexpected token {:?}", span, err);

        Err(Error {
            kind: ErrorKind::Unexpected(err),
            span,
            source: self.tokens.source().into(),
        })
    }

    fn error<T, E>(&self, err: E) -> Result<T>
    where
        E: Into<ErrorKind>,
    {
        traced!("error");
        let span = self.tokens.span().clone();
        Err(Error::new(err, span, self.tokens.source()))
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Precedence {
    Call = 7,
    UnaryLiteral = 6,
    BinaryMul = 5,
    BinaryAdd = 4,
    Relative = 3,
    UnaryBool = 2,
    BinaryBool = 1,
}

pub(crate) trait BinaryOp {
    fn as_binary_op(&self) -> Option<BinaryOperator>;
}

impl BinaryOp for Symbol {
    fn as_binary_op(&self) -> Option<BinaryOperator> {
        let res = match self {
            Symbol::Plus => BinaryOperator::Plus,
            Symbol::Minus => BinaryOperator::Minus,
            Symbol::Mul => BinaryOperator::Mul,
            Symbol::Div => BinaryOperator::Div,

            Symbol::LessThan => BinaryOperator::LessThan,
            Symbol::GreaterThan => BinaryOperator::GreaterThan,
            Symbol::LessThanEqual => BinaryOperator::LessThanEqual,
            Symbol::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
            Symbol::Equal => BinaryOperator::Equal,
            Symbol::NotEqual => BinaryOperator::NotEqual,
            _ => return None,
        };
        Some(res)
    }
}

impl BinaryOp for Reserved {
    fn as_binary_op(&self) -> Option<BinaryOperator> {
        Some(match self {
            Reserved::And => BinaryOperator::And,
            Reserved::Or => BinaryOperator::Or,
            Reserved::Div => BinaryOperator::Div,
            _ => return None,
        })
    }
}
