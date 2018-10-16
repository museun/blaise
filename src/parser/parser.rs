use super::*;

pub struct Parser {
    pub(crate) tokens: Tokens,
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
        let mut vars = vec![];
        if let Token::Reserved(token::Reserved::Var) = self.tokens.peek() {
            self.tokens.advance();
            while let Token::Identifier(_) = self.tokens.peek() {
                vars.push(self.variable_declaration()?)
            }
            if vars.is_empty() {
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

        decls.push(Declaration::Variable(vars));
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
        let block = self.block()?;
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
        let block = self.block()?;
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
        // begin stmt1; stmt2 end
        self.expect_token("begin")?;
        let mut statements = vec![self.statement()?];
        while !self.consume("end") {
            self.expect_token(";")?;
            statements.push(self.statement()?);
        }
        Ok(Compound(statements))
    }

    fn statement(&mut self) -> Result<Statement> {
        traced!("statement");
        use self::Statement::*;

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
            Token::Reserved(Reserved::If) => IfStatement(Box::new(self.if_statement()?)),
            Token::Reserved(Reserved::End) => Empty,

            Token::Reserved(Reserved::Repeat)
            | Token::Reserved(Reserved::While)
            | Token::Reserved(Reserved::For) => Repetitive(self.repetitive()?),
            t => self.unexpected(t)?,
        };
        Ok(res)
    }

    fn function_call(&mut self) -> Result<FunctionCall> {
        traced!("function_call");
        let id = self.expect(Self::variable, "(")?;
        let params = self.expect(Self::call_params, ")")?;
        Ok(FunctionCall(id, params))
    }

    // TODO fn statement_sequence(&mut self, end: impl Into<Token>) -> Vec<Statement> {}
    fn repetitive(&mut self) -> Result<Repetitive> {
        match self.tokens.current() {
            Token::Reserved(Reserved::Repeat) => {
                self.tokens.advance();
                // repeat stmt-seq until bool-expr
                let mut statements = vec![self.statement()?];
                while !self.consume("until") {
                    self.expect_token(";")?;
                    statements.push(self.statement()?);
                }
                // TODO check to see if this is boolean
                let expr = self.expression(None)?;
                Ok(Repetitive::Repeat(statements, expr))
            }
            Token::Reserved(Reserved::While) => {
                self.tokens.advance();
                // TODO check to see if this is boolean
                let expr = self.expression(None)?;
                self.expect_token("do")?;
                let comp = self.compound_statement()?;
                Ok(Repetitive::While(expr, comp))
            }
            Token::Reserved(Reserved::For) => unimplemented!(),
            _ => unreachable!(),
        }
    }

    fn if_statement(&mut self) -> Result<IfStatement> {
        traced!("if_statement");
        // if_stmt ::= IF expr THEN comp_stmt (ELSE (if_stmt | comp_stmt))?
        self.expect_token("if")?;
        let expr = self.expression(None)?;

        self.expect_token("then")?;
        let statement = self.statement()?;

        let res = if self.consume("else") {
            if self.peek("if") {
                // don't eat the if
                IfStatement::IfElseIf(expr, statement, Box::new(self.if_statement()?))
            } else {
                IfStatement::IfElse(expr, statement, self.statement()?)
            }
        } else {
            IfStatement::If(expr, statement)
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
        let expr = self.expression(None)?;
        Ok(Assignment(var, expr))
    }

    fn variable(&mut self) -> Result<Variable> {
        traced!("variable");
        let id = self.identifier()?;
        Ok(Variable(id))
    }

    fn ty(&mut self) -> Result<Type> {
        traced!("ty");
        match self.tokens.next_token() {
            Token::Type(t) => Ok(t.into()),
            t => self.unexpected(t)?,
        }
    }

    pub(crate) fn expression(&mut self, p: Option<u32>) -> Result<Expression> {
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

    pub(crate) fn function_call_expr(&mut self, left: Expression) -> Result<FunctionCall> {
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

    pub(crate) fn variable_expr(&mut self) -> Result<Variable> {
        traced!("variable_expr");
        match self.tokens.current() {
            Token::Identifier(id) => Ok(Variable(id.clone())),
            t => self.unexpected(t),
        }
    }

    pub(crate) fn unary_op(&mut self) -> Result<UnaryExpression> {
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

    pub(crate) fn literal(&mut self) -> Result<Literal> {
        traced!("literal");
        Ok(match self.tokens.current() {
            Token::Number(n) => Literal::Integer(n),
            Token::Real(n) => Literal::Real(n),
            Token::Reserved(Reserved::True) => Literal::Boolean(true),
            Token::Reserved(Reserved::False) => Literal::Boolean(false),
            Token::String(s) => Literal::String(s.clone()),
            t => self.unexpected(t)?,
        })
    }

    pub(crate) fn grouping(&mut self) -> Result<GroupExpression> {
        match self.tokens.current() {
            Token::Symbol(Symbol::OpenParen) => {
                let expr = self.expression(None)?;
                match self.tokens.next_token() {
                    Token::Symbol(Symbol::CloseParen) => Ok(GroupExpression(expr)),
                    t => self.unexpected(t),
                }
            }
            t => self.unexpected(t),
        }
    }

    fn prefix_parser(&mut self, token: &Token) -> Option<PrefixParser> {
        traced!("prefix_parser");
        match token {
            Token::Number(_)
            | Token::String(_)
            | Token::Real(_)
            | Token::Reserved(Reserved::True)
            | Token::Reserved(Reserved::False) => Some(PrefixParser::Literal),

            Token::Symbol(Symbol::Plus) | Token::Symbol(Symbol::Minus) => {
                Some(PrefixParser::UnaryOperator(Precedence::UnaryLiteral))
            }

            Token::Reserved(Reserved::Not) => {
                Some(PrefixParser::UnaryOperator(Precedence::UnaryBool))
            }

            Token::Symbol(Symbol::OpenParen) => Some(PrefixParser::Grouping),

            Token::Identifier(_) => Some(PrefixParser::Variable),
            _ => None,
        }
    }

    fn infix_parser(&mut self, token: &Token) -> Option<InfixParser> {
        traced!("infix_parser");
        use self::InfixParser::{BinaryOperator as BinOp, RelationalOperator as RelOp};
        use self::Precedence::*;
        use crate::prelude::token::{Reserved::*, Symbol::*};

        match token {
            Token::Symbol(Plus) | Token::Symbol(Minus) => Some(BinOp(BinaryAdd)),

            Token::Symbol(Symbol::Mul)
            | Token::Symbol(Symbol::Div)
            | Token::Reserved(Reserved::Div) => Some(BinOp(BinaryMul)),

            // is this relational?
            Token::Reserved(And) | Token::Reserved(Or) => Some(RelOp(BinaryBool)),

            Token::Symbol(LessThan)
            | Token::Symbol(GreaterThan)
            | Token::Symbol(LessThanEqual)
            | Token::Symbol(GreaterThanEqual)
            | Token::Symbol(Equal)
            | Token::Symbol(NotEqual) => Some(RelOp(Relative)),

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

    pub(crate) fn consume(&mut self, tok: impl Into<Token>) -> bool {
        let tok = tok.into();
        traced!("consume", "{:?}", tok);
        match self.tokens.peek() {
            ref t if *t == tok => {
                self.tokens.advance();
                true
            }
            _ => false,
        }
    }

    /// doesn't consume
    pub(crate) fn peek(&mut self, tok: impl Into<Token>) -> bool {
        let tok = tok.into();
        traced!("peek", "{:?}", tok);
        match self.tokens.peek() {
            ref t if *t == tok => true,
            _ => false,
        }
    }

    pub(crate) fn expect<E, F>(&mut self, mut f: F, tok: impl Into<Token>) -> Result<E>
    where
        F: FnMut(&mut Parser) -> Result<E>,
    {
        let tok = tok.into();
        traced!("expect", "{:?}", tok);
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
    pub(crate) fn expect_token(&mut self, tok: impl Into<Token>) -> Result<Token> {
        let tok = tok.into();
        traced!("expect_token", "{:?}", tok);
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
    pub(crate) fn expected<T>(&self, err: impl Into<ErrorKind>) -> Result<T> {
        traced!("expected");
        self.error(err)
    }

    pub(crate) fn unexpected<T, E>(&self, err: E) -> Result<T>
    where
        E: Into<Token> + Clone,
    {
        let span = self.tokens.span().clone();
        let err = err.into().clone();
        traced!("unexpected", "{:?}", err);
        debug!("{}: unexpected token {:?}", span, err);
        Err(Error {
            kind: ErrorKind::Unexpected(err),
            span,
            source: self.tokens.source().into(),
        })
    }

    pub(crate) fn error<T, E>(&self, err: E) -> Result<T>
    where
        E: Into<ErrorKind>,
    {
        let span = self.tokens.span().clone();
        let err = err.into();
        traced!("error", "{:?}", err);
        Err(Error::new(err, span, self.tokens.source()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_eq {
        ($left:expr, $right:expr,) => {
            assert_eq!($left, $right)
        };
        ($left:expr, $right:expr) => {
            match (&($left), &($right)) {
                (l, r) => {
                    if !(*l == *r) {
                        let l = format!("{:#?}", l);
                        let r = format!("{:#?}", r);
                        ::std::fs::write("diff_r.txt", l.as_bytes()).expect("write");
                        ::std::fs::write("diff_l.txt", r.as_bytes()).expect("write");

                        panic!("{}\n---\n{}", l, r)
                    }
                }
            }
        };
    }

    fn make_parser(input: &str) -> Parser {
        // TODO put this behind a feature flag

        // let _ = env_logger::Builder::from_default_env()
        //     .default_format_timestamp(false)
        //     .try_init();

        // enable_colors();
        // enable_tracer();

        let tokens = scan("", input);
        eprintln!("{}", tokens);
        Parser::new(tokens)
    }

    #[test]
    fn program() {
        let mut parser = make_parser("program test; begin end.");
        assert_eq!(
            parser.program(),
            Ok(Program(Variable("test".into()), Block::default()))
        );
    }

    #[test]
    fn block() {
        let mut parser = make_parser("begin end");
        assert_eq!(parser.block(), Ok(Block::default()));
    }

    #[test]
    fn declarations() {
        let input = "var a: integer; function b(): string; begin end procedure c; begin end";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.declarations(),
            Ok(vec![
                Declaration::Variable(vec![VariableDeclaration(vec!["a".into()], Type::Integer)]),
                Declaration::Procedure(vec![ProcedureDeclaration(
                    "c".into(),
                    FormalParameterList::default(),
                    Block::default(),
                )]),
                Declaration::Function(vec![FunctionDeclaration(
                    "b".into(),
                    FormalParameterList::default(),
                    Block::default(),
                    Type::String,
                )]),
            ])
        );
    }

    #[test]
    fn procedure_declaration() {
        let input = "procedure test(); begin end";
        let mut parser = make_parser(input);

        let mut p = ProcedureDeclaration::default();
        p.0 = "test".into();

        assert_eq!(parser.procedure_declaration(), Ok(p));
    }

    #[test]
    fn function_declaration() {
        let input = "function test(): integer; begin end";
        let mut parser = make_parser(input);

        let mut f = FunctionDeclaration::default();
        f.0 = "test".into();
        f.3 = Type::Integer;

        assert_eq!(parser.function_declaration(), Ok(f));
    }

    #[test]
    fn variable_declaration() {
        let input = "foo, bar, baz : integer;";
        let mut parser = make_parser(input);

        assert_eq!(
            parser.variable_declaration(),
            Ok(VariableDeclaration(
                vec!["foo", "bar", "baz"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
                Type::Integer
            ))
        );
    }

    #[test]
    fn compound_statement() {
        let input = "begin end";
        let mut parser = make_parser(input);
        assert_eq!(parser.compound_statement(), Ok(Compound::default()));
    }

    #[test]
    fn assignment_statement() {
        let input = "foo := 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.assignment_statement(),
            Ok(Assignment(
                Variable("foo".into()),
                Expression::Literal(Literal::Integer(5))
            ))
        );
    }

    #[test]
    fn repeat_statement() {
        let input = r#"
        repeat
            x := x + 1;
            x := x + 1
        until x = 10;
        "#;
        let mut parser = make_parser(input);
        assert_eq!(
            parser.statement(),
            Ok(Statement::Repetitive(Repetitive::Repeat(
                vec![
                    Statement::Assignment(Assignment(
                        Variable("x".into()),
                        Expression::Binary(Box::new(BinaryExpression(
                            Expression::Variable(Variable("x".into())),
                            BinaryOperator::Plus,
                            Expression::Literal(Literal::Integer(1)),
                        )))
                    )),
                    Statement::Assignment(Assignment(
                        Variable("x".into()),
                        Expression::Binary(Box::new(BinaryExpression(
                            Expression::Variable(Variable("x".into())),
                            BinaryOperator::Plus,
                            Expression::Literal(Literal::Integer(1)),
                        )))
                    ))
                ],
                Expression::Boolean(Box::new(BinaryExpression(
                    Expression::Variable(Variable("x".into())),
                    BinaryOperator::Equal,
                    Expression::Literal(Literal::Integer(10)),
                )))
            )))
        );
    }

    #[test]
    fn while_statement() {
        let input = r#"
        while x < 10 do
        begin
            x := x + 1
        end;            
        "#;
        let mut parser = make_parser(input);
        assert_eq!(
            parser.statement(),
            Ok(Statement::Repetitive(Repetitive::While(
                Expression::Boolean(Box::new(BinaryExpression(
                    Expression::Variable(Variable("x".into())),
                    BinaryOperator::LessThan,
                    Expression::Literal(Literal::Integer(10)),
                ))),
                Compound(vec![Statement::Assignment(Assignment(
                    Variable("x".into()),
                    Expression::Binary(Box::new(BinaryExpression(
                        Expression::Variable(Variable("x".into())),
                        BinaryOperator::Plus,
                        Expression::Literal(Literal::Integer(1)),
                    ))),
                ))]),
            )))
        )
    }

    #[test]
    fn if_statement() {
        let input = "if true then begin end";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.if_statement(),
            Ok(IfStatement::If(
                Expression::Literal(Literal::Boolean(true)),
                Statement::Compound(Compound::default())
            ))
        );
    }

    #[test]
    fn if_else_statement() {
        let input = "if false then begin end else begin end";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.if_statement(),
            Ok(IfStatement::IfElse(
                Expression::Literal(Literal::Boolean(false)),
                Statement::Compound(Compound::default()),
                Statement::Compound(Compound::default())
            ))
        );
    }

    #[test]
    fn if_else_if_statement() {
        let input = r#"if false then 
                begin end 
            else if true then
                begin end 
            else if true then 
                begin end"#;

        let mut parser = make_parser(input);
        assert_eq!(
            parser.if_statement(),
            Ok(IfStatement::IfElseIf(
                Expression::Literal(Literal::Boolean(false)),
                Statement::Compound(Compound::default()),
                Box::new(IfStatement::IfElseIf(
                    Expression::Literal(Literal::Boolean(true)),
                    Statement::Compound(Compound::default()),
                    Box::new(IfStatement::If(
                        Expression::Literal(Literal::Boolean(true)),
                        Statement::Compound(Compound::default())
                    ))
                ))
            ))
        );
    }

    #[test]
    fn if_else_if_else_statement() {
        let input = r#"if false then 
                begin end 
            else if true then
                begin end 
            else if true then 
                begin end
            else begin end"#;

        let mut parser = make_parser(input);
        assert_eq!(
            parser.if_statement(),
            Ok(IfStatement::IfElseIf(
                Expression::Literal(Literal::Boolean(false)),
                Statement::Compound(Compound::default()),
                Box::new(IfStatement::IfElseIf(
                    Expression::Literal(Literal::Boolean(true)),
                    Statement::Compound(Compound::default()),
                    Box::new(IfStatement::IfElse(
                        Expression::Literal(Literal::Boolean(true)),
                        Statement::Compound(Compound::default()),
                        Statement::Compound(Compound::default())
                    ))
                ))
            ))
        );
    }

    #[test]
    fn variable() {
        let input = "foo";
        let mut parser = make_parser(input);
        assert_eq!(parser.variable(), Ok(Variable("foo".into())));
    }

    #[test]
    fn literal() {
        let input = "42";
        let mut parser = make_parser(input);
        assert_eq!(parser.literal(), Ok(Literal::Integer(42)));

        let input = "'foobar'";
        let mut parser = make_parser(input);
        assert_eq!(parser.literal(), Ok(Literal::String("foobar".into())));

        let input = "false";
        let mut parser = make_parser(input);
        assert_eq!(parser.literal(), Ok(Literal::Boolean(false)));

        for input in &[("1.234", 1.234f64), ("5e-3", 5e-3), ("87.35E+8", 87.35E+8)] {
            let mut parser = make_parser(input.0);
            assert_eq!(parser.literal(), Ok(Literal::Real(input.1)));
        }
    }

    #[test]
    fn call_params() {
        let input = "foo, bar, 21 * 2";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.call_params(),
            Ok(CallParams(vec![
                Expression::Variable(Variable("foo".into())),
                Expression::Variable(Variable("bar".into())),
                Expression::Binary(Box::new(BinaryExpression(
                    Expression::Literal(Literal::Integer(21)),
                    BinaryOperator::Mul,
                    Expression::Literal(Literal::Integer(2)),
                )))
            ]))
        );
    }

    #[test]
    fn function_call() {
        let input = "test(foo, bar, 21 * 2);";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.function_call(),
            Ok(FunctionCall(
                Variable("test".into(),),
                CallParams(vec![
                    Expression::Variable(Variable("foo".into())),
                    Expression::Variable(Variable("bar".into())),
                    Expression::Binary(Box::new(BinaryExpression(
                        Expression::Literal(Literal::Integer(21)),
                        BinaryOperator::Mul,
                        Expression::Literal(Literal::Integer(2)),
                    )))
                ])
            ))
        );
    }

    #[test]
    fn formal_parameter_list() {
        let input = "a: integer; b: string; c,d: bool; e,f: real";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.formal_parameter_list(),
            Ok(FormalParameterList(vec![
                FormalParameter(vec!["a".into()], Type::Integer),
                FormalParameter(vec!["b".into()], Type::String),
                FormalParameter(vec!["c".into(), "d".into()], Type::Boolean),
                FormalParameter(vec!["e".into(), "f".into()], Type::Real),
            ]))
        );
    }

    #[test]
    fn formal_parameter() {
        let input = "a,b,c : bool";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.formal_parameter(),
            Ok(FormalParameter(
                vec!["a".into(), "b".into(), "c".into()],
                Type::Boolean
            ),)
        );
    }

    #[test]
    fn expr_unary_plus() {
        let input = "+5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Unary(Box::new(UnaryExpression(
                UnaryOperator::Plus,
                Expression::Literal(Literal::Integer(5))
            ))))
        );
    }

    #[test]
    fn expr_unary_minus() {
        let input = "-5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Unary(Box::new(UnaryExpression(
                UnaryOperator::Minus,
                Expression::Literal(Literal::Integer(5))
            ))))
        );
    }

    #[test]
    fn expr_binary_plus() {
        let input = "5+5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Binary(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::Plus,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_minus() {
        let input = "5-5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Binary(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::Minus,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_mul() {
        let input = "5*5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Binary(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::Mul,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_div() {
        let input = "5 div 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Binary(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::Div,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_div_real() {
        let input = "5 / 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Binary(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::RealDiv,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_and() {
        let input = "5 and 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::And,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_or() {
        let input = "5 or 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::Or,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_lessthan() {
        let input = "5 < 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::LessThan,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_greaterthan() {
        let input = "5 > 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::GreaterThan,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_lessthanequal() {
        let input = "5 <= 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::LessThanEqual,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_greaterthanequal() {
        let input = "5 >= 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::GreaterThanEqual,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_equal() {
        let input = "5 = 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::Equal,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_notequal() {
        let input = "5 <> 5";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::NotEqual,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_function_call() {
        let input = "test()";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::FunctionCall(FunctionCall(
                Variable("test".into()),
                CallParams::default()
            )))
        );
    }

    #[test]
    fn expr_variable() {
        let input = "test";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Variable(Variable("test".into())))
        );
    }

    #[test]
    fn expr_literal() {
        for input in &[
            ("'test'", Literal::String("test".into())),
            ("5", Literal::Integer(5)),
            ("5.5", Literal::Real(5.5)),
            ("true", Literal::Boolean(true)),
        ] {
            let mut parser = make_parser(input.0);
            assert_eq!(
                parser.expression(None),
                Ok(Expression::Literal(input.1.clone()))
            );
        }
    }

    #[test]
    fn expr_grouped() {
        let input = "(5 + 5)";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Group(Box::new(GroupExpression(
                Expression::Binary(Box::new(BinaryExpression(
                    Expression::Literal(Literal::Integer(5)),
                    BinaryOperator::Plus,
                    Expression::Literal(Literal::Integer(5)),
                )))
            ))))
        );

        let input = "(5 + (5 - 5))";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Group(Box::new(GroupExpression(
                Expression::Binary(Box::new(BinaryExpression(
                    Expression::Literal(Literal::Integer(5)),
                    BinaryOperator::Plus,
                    Expression::Group(Box::new(GroupExpression(Expression::Binary(Box::new(
                        BinaryExpression(
                            Expression::Literal(Literal::Integer(5)),
                            BinaryOperator::Minus,
                            Expression::Literal(Literal::Integer(5))
                        )
                    )))))
                )))
            ))))
        )
    }

    #[test]
    fn ty() {
        let input = "integer";
        let mut parser = make_parser(input);
        assert_eq!(parser.ty(), Ok(Type::Integer));

        let input = "string";
        let mut parser = make_parser(input);
        assert_eq!(parser.ty(), Ok(Type::String));

        let input = "bool";
        let mut parser = make_parser(input);
        assert_eq!(parser.ty(), Ok(Type::Boolean));
    }

    #[test]
    fn identifier() {
        let input = "foobar";
        let mut parser = make_parser(input);
        assert_eq!(parser.identifier(), Ok("foobar".into()));
    }
}
