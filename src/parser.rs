use crate::ast::*;
use crate::span::Span;
use crate::tokens::*;

pub struct Parser<'a> {
    tokens: Tokens<'a>,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: Tokens<'a>) -> Program {
        let mut this = Self { tokens };
        let program = this.program();
        match this.tokens.next() {
            Some((_, Token::EOF)) => {}
            t => unimplemented!("{:#?}", t),
        }
        program
    }

    fn program(&mut self) -> Program {
        if !self.consume(Token::Reserved(Reserved::Program)) {
            panic!("expected program");
        }

        let variable = self.variable();
        if !self.consume(Token::Symbol(Symbol::SemiColon)) {
            panic!("expected semi-colon");
        }

        let block = self.block();
        if !self.consume(Token::Symbol(Symbol::Period)) {
            panic!("expected period");
        }

        Program(variable, block)
    }

    fn block(&mut self) -> Block {
        let decls = self.declarations();
        let compound = self.compound_statement();
        Block(decls, compound)
    }

    fn declarations(&mut self) -> Vec<Declaration> {
        let mut decls = vec![];
        if let Some((_, Token::Reserved(Reserved::Var))) = self.tokens.peek() {
            self.tokens.advance();
            let mut vars = vec![];
            while let Some((_, Token::Identifier(_))) = self.tokens.peek() {
                vars.push(self.variable_declaration())
            }

            if !vars.is_empty() {
                decls.push(Declaration::Variable(vars))
            }
        }
        if decls.is_empty() {
            decls.push(Declaration::Empty)
        }
        decls
    }

    fn variable_declaration(&mut self) -> VariableDeclaration {
        let mut idents = vec![];
        match self.tokens.next() {
            Some((_, Token::Identifier(id))) => idents.push(id),
            t => unimplemented!("{:#?}", t),
        };

        while let Some((_, Token::Symbol(Symbol::Comma))) = self.tokens.peek() {
            self.tokens.advance(); // eat comma
            match self.tokens.next() {
                Some((_, Token::Identifier(id))) => idents.push(id),
                t => unimplemented!("{:#?}", t),
            };
        }

        // var a,b,c,d : integer;
        match self.tokens.next() {
            Some((_, Token::Symbol(Symbol::Colon))) => match (self.ty(), self.tokens.next()) {
                (ty, Some((_, Token::Symbol(Symbol::SemiColon)))) => {
                    VariableDeclaration(idents, ty)
                }
                t => unimplemented!("expected ; after {:#?}", t),
            },
            t => unimplemented!("{:#?}", t),
        }
    }

    fn compound_statement(&mut self) -> Compound {
        match self.tokens.next() {
            Some((_, Token::Reserved(Reserved::Begin))) => {}
            t => unimplemented!("{:#?}", t),
        };

        let mut statements = vec![];
        while !self.consume(Token::Reserved(Reserved::End)) {
            statements.push(self.statement())
        }
        Compound(statements)
    }

    fn statement(&mut self) -> Statement {
        // TODO some impl Into<T> to make this less "noisey"
        match self.tokens.peek() {
            Some((_, Token::Reserved(Reserved::Begin))) => {
                Statement::Compound(self.compound_statement())
            }
            Some((_, Token::Identifier(_id))) => match self.tokens.peek_ahead(1) {
                t @ Some((_, Token::Symbol(Symbol::OpenParen))) => {
                    // function call
                    unimplemented!("{:#?}", t)
                }
                Some((_, Token::Symbol(Symbol::Assign))) => {
                    Statement::Assignment(self.assignment_statement())
                }
                t => {
                    // expected function call or assignment after identifier
                    unimplemented!("{:#?}", t)
                }
            },
            t => {
                // expected statement
                unimplemented!("{:#?}", t)
            }
        }
    }

    fn assignment_statement(&mut self) -> Assignment {
        match (self.variable(), self.tokens.next()) {
            (var, Some((_, Token::Symbol(Symbol::Assign)))) => {
                match (self.expression(None), self.tokens.next()) {
                    (expr, Some((_, Token::Symbol(Symbol::SemiColon)))) => Assignment(var, expr),
                    t => unimplemented!("expected ; after {:#?}", t),
                }
            }
            t => unimplemented!("expected = after {:#?}", t),
        }
    }

    fn expression(&mut self, p: Option<u32>) -> Expression {
        let p = p.unwrap_or(0);

        let token = match self.tokens.next() {
            Some((_, token)) => token,
            None => unimplemented!("unexpected eof"), // EOF
        };

        let parser = self.prefix_parser(&token).expect("prefix parser");
        let mut lhs = parser.parse(self);

        while p < self.next_precedence() {
            let (_, token) = self.tokens.next().expect("next token");
            let parser = self.infix_parser(&token).expect("infix parser");
            lhs = parser.parse(self, lhs);
        }

        lhs
    }

    fn variable(&mut self) -> Variable {
        match self.tokens.next() {
            Some((_, Token::Identifier(id))) => Variable(id),
            Some((span, token)) => expected(span, Token::Unknown, token),
            t => unimplemented!("{:#?}", t), // EOF
        }
    }

    fn variable_expr(&mut self) -> Variable {
        match self.tokens.current() {
            Some((_, Token::Identifier(id))) => Variable(id.clone()),
            Some((span, token)) => expected(*span, Token::Unknown, token.clone()),
            t => unimplemented!("{:#?}", t), // EOF
        }
    }

    fn unary_op(&mut self) -> UnaryExpression {
        let op = match self.tokens.current() {
            Some((_, Token::Symbol(Symbol::Plus))) => UnaryOperator::Plus,
            Some((_, Token::Symbol(Symbol::Minus))) => UnaryOperator::Minus,
            Some((_, Token::Reserved(Reserved::Not))) => UnaryOperator::Not,
            t => unimplemented!("{:#?}", t),
        };
        UnaryExpression(op, self.expression(None))
    }

    fn binary_op(&mut self, expr: Expression) -> BinaryExpression {
        let op = match self.tokens.current() {
            Some((_, Token::Symbol(Symbol::Plus))) => BinaryOperator::Plus,
            Some((_, Token::Symbol(Symbol::Minus))) => BinaryOperator::Minus,
            Some((_, Token::Symbol(Symbol::Mul))) => BinaryOperator::Mul,
            Some((_, Token::Symbol(Symbol::Div))) => BinaryOperator::Div,
            t => unimplemented!("{:#?}", t),
        };
        BinaryExpression(expr, op, self.expression(None))
    }

    fn literal(&mut self) -> Literal {
        let t = self.tokens.current();
        match t {
            Some((_, Token::Number(n))) => Literal::Integer(*n),
            Some((_, Token::String(s))) => Literal::String(s.clone()),
            t => unimplemented!("{:#?}", t),
        }
    }

    fn ty(&mut self) -> crate::ast::Type {
        match self.tokens.next() {
            Some((_, Token::Type(crate::tokens::Type::Integer))) => crate::ast::Type::Integer, // this isn't right
            Some((_, Token::Type(crate::tokens::Type::String))) => crate::ast::Type::String,
            t => unimplemented!("{:#?}", t),
        }
    }

    fn consume(&mut self, tok: Token) -> bool {
        match self.tokens.peek() {
            Some((_, t)) if *t == tok => {
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
        match self.tokens.peek() {
            Some((_, token)) => {
                let token = token.clone();
                match self.infix_parser(&token) {
                    Some(pp) => pp.precedence(),
                    _ => 0,
                }
            }
            _ => 0,
        }
    }

    // fn expect(&mut self, token: Token) {
    //     let next = self.tokens.peek();
    //     eprintln!("next: {:?} (expecting: {:?}", next.map(|(_, t)| t), token);
    //     match next {
    //         Some((_, ref tok)) if *tok == token => self.tokens.advance(),
    //         Some((span, tok)) => expected(*span, token, tok.clone()),
    //         t => unimplemented!("{:#?}", t), // EOF
    //     }
    // }
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
    // FunctionCall(u32),
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
        }
    }

    pub fn precedence(&self) -> u32 {
        match self {
            InfixParser::BinaryOperator(p) => *p,
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

fn expected(span: Span, token: Token, got: Token) -> ! {
    eprintln!("{} expected: {:?} got {:?}", span, token, got);
    panic!();
}

use std::borrow::Cow;
use std::sync::atomic::{AtomicUsize, Ordering};

const IDENTATION: usize = 4;
static TRACE_DEPTH: AtomicUsize = AtomicUsize::new(0);

fn indent() {
    let _ = TRACE_DEPTH.fetch_add(IDENTATION, Ordering::Relaxed);
}

fn deindent() {
    let _ = TRACE_DEPTH.fetch_sub(IDENTATION, Ordering::Relaxed);
}

fn reset_level() {
    TRACE_DEPTH.store(0, Ordering::Relaxed);
}

fn level() -> usize {
    TRACE_DEPTH.load(Ordering::Relaxed)
}

// for co-recursive shit
// macro_rules! tracer {
//     ($e:expr, $($args:tt)*) => {{
//         Tracer::new($e, format!($($args)*))
//     }};
// }

pub struct Tracer<'a> {
    label: Cow<'a, str>,
    pad: Cow<'a, str>,
}

impl<'a> Tracer<'a> {
    pub fn new(label: impl Into<String>, data: impl AsRef<str>) -> Self {
        let pad = ::std::iter::repeat(".")
            .take(level())
            .collect::<String>()
            .into();

        let lede = format!("{}>", pad);
        let label = label.into();
        eprintln!("{}{}: {}", lede, label, data.as_ref());
        indent();
        Tracer {
            label: label.into(),
            pad,
        }
    }
}

impl<'a> Drop for Tracer<'a> {
    fn drop(&mut self) {
        deindent();
        let lede = format!("<{}", self.pad);
        eprintln!("{}{}", lede, self.label)
    }
}
