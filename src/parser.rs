use crate::ast::*;
use crate::span::Span;
use crate::tokens::*;

use std::borrow::Cow;
use std::fmt;

pub struct Error {
    kind: ErrorKind,
    span: Span,
    source: String,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let line = self.source.lines().nth(self.span.row() - 1).unwrap();
        let (data, adjusted) = midpoint(line, self.span.column() - 1, 80);
        writeln!(f, "{}", data)?;
        writeln!(f, "{}", draw_caret(adjusted));
        write!(f, "{} {:?}", self.span, self.kind)
    }
}

impl Error {
    pub fn new(kind: impl Into<ErrorKind>, span: Span, src: impl Into<String>) -> Self {
        Self {
            kind: kind.into(),
            span,
            source: src.into(),
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Unknown(String),
    Expected(Token),
    Unexpected(Token),
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

type Result<T> = ::std::result::Result<T, Error>;

pub struct Parser {
    tokens: Tokens,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Self {
        Self { tokens }
    }

    pub fn parse(mut self) -> Result<Program> {
        let program = self.program()?;
        let mut tokens = self.tokens;
        match tokens.next() {
            Some(Token::EOF) => Ok(program),
            Some(t) => Err(Error {
                kind: ErrorKind::Unexpected(t),
                span: tokens.span().clone(),
                source: tokens.source().into(),
            }),
            None => Err(Error::new(
                Token::EOF,
                tokens.span().clone(),
                tokens.source(),
            )),
        }
    }

    pub fn program(&mut self) -> Result<Program> {
        self.expect_token("program")?;
        let variable = self.expect(Self::variable, &[";"]).map_err(|err| {
            error!("want: variable:\n{:?}", err);
            err
        })?;
        let block = self
            .expect(Self::block, &["."])
            .map_err(|err| {
                error!("want: block:\n{:?}", err);
                err
            })
            .expect("gimme a stack trace please");
        Ok(Program(variable, block))
    }

    pub fn block(&mut self) -> Result<Block> {
        let decls = self.declarations().map_err(|err| {
            error!("want: decls:\n{:?}", err);
            err
        })?;

        trace!("decls: {:#?}", decls);
        let compound = self.compound_statement().map_err(|err| {
            error!("want: compound:\n{:?}", err);
            err
        })?;
        Ok(Block(decls, compound))
    }

    fn declarations(&mut self) -> Result<Vec<Declaration>> {
        let mut decls = vec![];
        if let Token::Reserved(Reserved::Var) = self.tokens.peek() {
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
        // proc decl ::= PROC ID (LPAREN, list, RPAREN)? SEMI block
        self.expect_token("procedure")?;
        // TODO proc decl error
        let name = self.identifier()?;

        let params = match self.tokens.peek() {
            Token::Symbol(Symbol::OpenParen) => {
                self.tokens.advance();
                self.expect(Self::formal_parameter_list, &[")"])?
            }
            _ => FormalParameterList(vec![]),
        };

        self.expect_token(";")?;
        let block = self.expect(Self::block, &[";"])?;
        Ok(ProcedureDeclaration(name, params, block))
    }

    fn function_declaration(&mut self) -> Result<FunctionDeclaration> {
        // func decl ::= FUNC ID LPAREN list RPAREN COLON type SEMI block
        self.expect_token("function")?;
        // TODO func decl error
        let name = self.identifier()?;

        let params = match self.tokens.peek() {
            Token::Symbol(Symbol::OpenParen) => {
                self.tokens.advance();
                self.expect(Self::formal_parameter_list, &[")"])?
            }
            _ => FormalParameterList(vec![]),
        };

        self.expect_token(":")?;
        let ty = self.expect(Self::ty, &[";"])?;
        let block = self.expect(Self::block, &[";"])?;
        Ok(FunctionDeclaration(name, params, block, ty))
    }

    fn variable_declaration(&mut self) -> Result<VariableDeclaration> {
        // var a,b,c,d : integer;
        // TODO identifier name
        let mut idents = vec![self.identifier()?];
        while self.consume(",") {
            idents.push(self.identifier()?);
        }

        self.expect_token(":")?;
        let ty = self.expect(Self::ty, &[";"])?;
        Ok(VariableDeclaration(idents, ty))
    }

    fn compound_statement(&mut self) -> Result<Compound> {
        // begin stmt1; stmt2; end
        self.expect_token("begin").unwrap();
        let mut statements = vec![];
        while !self.consume("end") {
            statements.push(self.statement().unwrap())
        }
        Ok(Compound(statements))
    }

    fn statement(&mut self) -> Result<Statement> {
        use crate::ast::Statement::*;
        use crate::tokens::{
            Reserved::{self, *},
            Symbol::{self, *},
            Token::*,
        };

        let res = match self.tokens.peek() {
            Reserved(Begin) => Compound(self.compound_statement()?),
            Identifier(_) => match self.tokens.peek_ahead(1).unwrap() {
                Symbol(OpenParen) => FunctionCall(self.function_call()?),
                Symbol(Assign) => Assignment(self.assignment_statement()?),
                t => self.unexpected(t)?,
            },
            Reserved(If) => IfStatement(self.if_statement()?),
            t => self.unexpected(t)?,
        };

        Ok(res)
    }

    fn function_call(&mut self) -> Result<FunctionCall> {
        let id = self.expect(Self::variable, &["("])?;
        let params = self.expect(Self::call_params, &[")", ";"])?;
        Ok(FunctionCall(id, params))
    }

    fn function_call_expr(&mut self, left: &Expression) -> Result<FunctionCall> {
        let name = match left {
            Expression::Variable(name) => name,
            e => self.error("expression isn't a variable")?,
        };

        debug!("call expr: {:?}", name);
        let params = if self.peek("(") {
            self.tokens.advance();
            self.call_params()?
        } else {
            CallParams(vec![])
        };
        debug!("call args: {:?}", params);
        Ok(FunctionCall(name.clone(), params))
    }

    fn if_statement(&mut self) -> Result<IfStatement> {
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
        debug!("call params: {:#?}", self.tokens.peek());
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
        let mut idents = vec![self.identifier()?];
        while self.consume(",") {
            idents.push(self.identifier()?)
        }

        self.expect_token(":")?;
        let ty = self.ty()?;
        Ok(FormalParameter(idents, ty))
    }

    fn assignment_statement(&mut self) -> Result<Assignment> {
        let var = self.expect(Self::variable, &[":="])?;
        let expr = self.expect(|p| Self::expression(p, None), &[";"])?;
        Ok(Assignment(var, expr))
    }

    fn expression(&mut self, p: Option<u32>) -> Result<Expression> {
        let p = p.unwrap_or(0);
        let token = self.tokens.next_token();
        debug!("expr tok: {:#?}", token);
        let parser = self
            .prefix_parser(&token)
            .ok_or_else(|| self.error::<(), _>(token).unwrap_err())?;
        let mut lhs = parser.parse(self)?;

        while p < self.next_precedence() {
            let token = self.tokens.peek();
            let parser = self.infix_parser(&token).expect("infix parser");
            lhs = parser.parse(self, lhs)?;
        }

        Ok(lhs)
    }

    fn variable(&mut self) -> Result<Variable> {
        let id = self.identifier()?;
        Ok(Variable(id))
    }

    fn variable_expr(&mut self) -> Result<Variable> {
        match self.tokens.current() {
            Token::Identifier(id) => Ok(Variable(id.clone())),
            t => self.unexpected(t),
        }
    }

    fn unary_op(&mut self) -> Result<UnaryExpression> {
        let op = match self.tokens.current() {
            Token::Symbol(Symbol::Plus) => UnaryOperator::Plus,
            Token::Symbol(Symbol::Minus) => UnaryOperator::Minus,
            Token::Reserved(Reserved::Not) => UnaryOperator::Not,
            t => self.unexpected(t)?,
        };
        Ok(UnaryExpression(op, self.expression(None)?))
    }

    fn binary_op(&mut self, expr: Expression) -> Result<BinaryExpression> {
        use self::BinaryOperator as Op;
        use crate::tokens::{
            Reserved::*,
            Symbol::*,
            Token::{Reserved, Symbol},
        };

        let t = self.tokens.current();
        let op = match t {
            Symbol(s) => s
                .as_binary_op()
                .ok_or_else(|| self.unexpected::<(), _>(t).unwrap_err())?,

            Reserved(s) => s
                .as_binary_op()
                .ok_or_else(|| self.unexpected::<(), _>(t).unwrap_err())?,

            t => self.unexpected(t)?,
        };
        Ok(BinaryExpression(expr, op, self.expression(None)?))
    }

    fn literal(&mut self) -> Result<Literal> {
        Ok(match self.tokens.current() {
            Token::Number(n) => Literal::Integer(n),
            Token::String(s) => Literal::String(s.clone()),
            t => self.unexpected(t)?,
        })
    }

    fn ty(&mut self) -> Result<crate::ast::Type> {
        trace!("entering ty");
        match self.tokens.next_token() {
            Token::Type(t) => {
                let t = t.into();
                trace!("leaving ty with: {:?}", t);
                Ok(t)
            }
            t => self.unexpected(t)?,
        }
    }

    fn prefix_parser(&mut self, token: &Token) -> Option<PrefixParser> {
        trace!("prefix_parser: {:#?}", token);
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
        use self::InfixParser::BinaryOperator as Op;
        use self::Precendence::*;
        use crate::tokens::{
            Reserved::{self, *},
            Symbol::{self, *},
            Token::*,
        };

        match token {
            Symbol(Plus) | Symbol(Minus) => Some(Op(Addition as u32)),
            Symbol(Mul) | Symbol(Symbol::Div) => Some(Op(Multiplication as u32)),
            Reserved(And) | Reserved(Or) => Some(Op(BinaryBool as u32)),

            Symbol(LessThan)
            | Symbol(GreaterThan)
            | Symbol(LessThanEqual)
            | Symbol(GreaterThanEqual)
            | Symbol(Equal)
            | Symbol(NotEqual) => Some(Op(Relative as u32)),

            Symbol(OpenParen) => Some(InfixParser::FunctionCall(Call as u32)),
            _ => None,
        }
    }

    fn next_precedence(&mut self) -> u32 {
        let token = self.tokens.peek().clone();
        match self.infix_parser(&token) {
            Some(pp) => pp.precedence(),
            _ => 0,
        }
    }

    fn consume(&mut self, tok: impl Into<Token>) -> bool {
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
        let tok = tok.into();
        match self.tokens.peek() {
            ref t if *t == tok => true,
            _ => false,
        }
    }

    fn expect<E, T, F>(&mut self, mut f: F, toks: impl AsRef<[T]>) -> Result<E>
    where
        T: Into<Token> + Clone,
        F: FnMut(&mut Parser) -> Result<E>,
    {
        let res = f(self).unwrap();
        for tok in toks.as_ref() {
            self.expect_token(tok.clone()).unwrap();
        }
        Ok(res)
    }

    // maybe this should peek
    fn expect_token(&mut self, tok: impl Into<Token>) -> Result<Token> {
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

    fn identifier(&mut self) -> Result<String> {
        match self.tokens.next().ok_or_else(|| Error {
            kind: ErrorKind::Unexpected(Token::EOF),
            span: self.tokens.span().clone(),
            source: self.tokens.source().into(),
        })? {
            Token::Identifier(name) => Ok(name.clone()),
            t => self.unexpected(t)?,
        }
    }

    // just to give it a better name
    fn expected<T>(&self, err: impl Into<ErrorKind>) -> Result<T> {
        self.error(err)
    }

    fn unexpected<T, E>(&self, err: E) -> Result<T>
    where
        E: Into<Token> + Clone,
    {
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
        let span = self.tokens.span().clone();
        Err(Error::new(err, span, self.tokens.source()))
    }
}

trait BinaryOp {
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
            _ => return None,
        })
    }
}

// Operator precedences shall be according to four classes of operators
// as follows. The operator not shall have the highest precedence,
// followed by the multiplying-operators, then the adding-operators and
// signs, and finally, with the lowest precedence, the relational-
// operators. Sequences of two or more operators of the same precedence
// shall be left associative.

#[derive(Debug)]
enum PrefixParser {
    Literal,
    Variable,
    UnaryOperator(u32),
    // grouping (...)
}

impl PrefixParser {
    pub fn parse(&self, parser: &mut Parser) -> Result<Expression> {
        Ok(match self {
            PrefixParser::Literal => Expression::Literal(parser.literal()?),
            PrefixParser::Variable => Expression::Variable(parser.variable_expr()?),
            PrefixParser::UnaryOperator(_) => Expression::Unary(Box::new(parser.unary_op()?)),
        })
    }

    pub fn precedence(&self) -> u32 {
        if let PrefixParser::UnaryOperator(p) = *self {
            return p;
        }
        0
    }
}

#[derive(Debug)]
enum InfixParser {
    BinaryOperator(u32),
    FunctionCall(u32),
}

impl InfixParser {
    pub fn parse(&self, parser: &mut Parser, left: Expression) -> Result<Expression> {
        debug!("infix parser: {:#?}", self);
        Ok(match self {
            InfixParser::BinaryOperator(_) => Expression::Binary(Box::new(parser.binary_op(left)?)),
            InfixParser::FunctionCall(_) => {
                Expression::FunctionCall(parser.function_call_expr(&left)?)
            }
        })
    }

    pub fn precedence(&self) -> u32 {
        match *self {
            InfixParser::BinaryOperator(p) | InfixParser::FunctionCall(p) => p,
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

fn midpoint(input: &str, cursor: usize, width: usize) -> (&str, usize) {
    let half = width / 2;
    if input.len() > width {
        if cursor < half {
            (&input[..half], cursor)
        } else {
            (&input[cursor - half..], half)
        }
    } else {
        (input, cursor)
    }
}

fn draw_caret(width: usize) -> String {
    let s = ::std::iter::repeat(" ").take(width).collect::<String>();
    format!("{}^", s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::*;

    fn new_parser(input: &str) -> Parser {
        let mut tokens = Lexer::scan("stdin", &input);
        tokens.remove_comments();
        Parser::new(tokens)
    }

    macro_rules! check {
        ($l:expr, $($p:pat)|*) => {
            match $l {
                $($p)|* => (),
                ref result => panic!("expected: {}, got: {:?}", stringify!($($p)|*), result),
            }
        };
    }

    #[test]
    fn program() {
        let _ = env_logger::Builder::from_default_env()
            .default_format_timestamp(false)
            .try_init();

        let input = r#"
        program test;
        begin 
        end.
        "#;

        let mut parser = new_parser(input);
        let ast = parser.program().expect("to parse a program");

        check!(ast, Program(_, _));
        eprintln!("{:#?}", ast);
    }
}
