use super::*;
use std::rc::Rc;

macro_rules! traced {
    ($this:expr, $name:expr) => {
        let _t = $this.tracer.as_ref().map(Rc::clone);
        let _t = _t.as_ref().map(|s| s.trace($name));
    };

    ($this:expr, $name:expr, $fmt:expr, $($arg:tt)*) => {
        let _t = $this.tracer.as_ref().map(Rc::clone);
        let _t = _t.as_ref().map(|s| s.trace(format!($fmt, $($arg)*)));
    };
}

pub struct Parser {
    tokens: Tokens,
    source: String,
    filename: String,
    tracer: Option<Rc<Tracer>>,
}

impl Parser {
    pub fn new(
        tokens: Tokens,
        source: impl Into<String>,
        filename: impl Into<String>,
        tracer: Option<Tracer>,
    ) -> Self {
        Self {
            tokens,
            source: source.into(),
            filename: filename.into(),
            tracer: tracer.map(Rc::new),
        }
    }

    pub fn parse(mut self) -> Result<Program> {
        traced!(self, "parse");

        let program = self.program()?;
        self.advance();
        match self.current() {
            Ok(TokenType::EOF)
            | Err(Error {
                kind: ErrorKind::Unexpected(TokenType::EOF),
                ..
            }) => Ok(program),

            // unexpected token
            Ok(t) => Err(Error::new(
                ErrorKind::Unexpected(t),
                self.tokens.span(),
                self.source,
                self.filename,
            )),

            Err(err) => Err(err),
        }
    }

    pub fn program(&mut self) -> Result<Program> {
        traced!(self, "program");

        self.expect("program")?;

        let var = self.variable()?;
        self.expect(";")?;

        let block = self.block()?;
        self.expect(".")?;

        Ok(Program(var, block))
    }

    fn block(&mut self) -> Result<Block> {
        traced!(self, "block");

        let decls = self.declarations()?;
        let compound = self.compound_statement()?;

        Ok(Block(decls, compound))
    }

    fn declarations(&mut self) -> Result<Vec<Declaration>> {
        traced!(self, "declarations");

        let mut vars = vec![];
        if let TokenType::Var = self.current()? {
            self.advance();
            while let TokenType::Identifier(_) = self.current()? {
                vars.push(self.variable_declaration()?)
            }
            if vars.is_empty() {
                self.error("var should be followed by variable declarations")?
            }
        }

        let (mut procs, mut funcs) = (vec![], vec![]);
        loop {
            match self.current()? {
                TokenType::Procedure => {
                    procs.push(self.procedure_declaration()?);
                    if let TokenType::SemiColon = self.current()? {
                        self.advance();
                    }
                }
                TokenType::Function => {
                    funcs.push(self.function_declaration()?);
                    if let TokenType::SemiColon = self.current()? {
                        self.advance();
                    }
                }
                _ => break,
            };
        }

        let mut decls = vec![];
        decls.push(Declaration::Variable(vars));
        decls.push(Declaration::Procedure(procs));
        decls.push(Declaration::Function(funcs));

        Ok(decls)
    }

    fn variable_declaration(&mut self) -> Result<VariableDeclaration> {
        traced!(self, "variable_declaration");

        let mut idents = vec![self.identifier()?];
        while self.consume(",") {
            idents.push(self.identifier()?);
        }
        self.expect(":")?;

        let ty = self.ty()?;
        self.expect(";")?;

        Ok(VariableDeclaration(idents, ty))
    }

    fn procedure_declaration(&mut self) -> Result<ProcedureDeclaration> {
        traced!(self, "procedure_declaration");

        self.expect("procedure")?;
        let name = self.identifier()?;

        let params = match self.current() {
            Ok(TokenType::OpenParen) => {
                self.advance();
                let list = self.formal_parameter_list()?;
                self.expect(")")?;
                list
            }
            _ => FormalParameterList::default(),
        };

        self.expect(";")?;
        let block = self.block()?;

        Ok(ProcedureDeclaration(name, params, block))
    }

    fn function_declaration(&mut self) -> Result<FunctionDeclaration> {
        traced!(self, "function_declaration");

        self.expect("function")?;
        let name = self.identifier()?;

        let params = match self.current() {
            Ok(TokenType::OpenParen) => {
                self.advance();
                let list = self.formal_parameter_list()?;
                self.expect(")")?;
                list
            }
            _ => FormalParameterList::default(),
        };

        self.expect(":")?;
        let ty = self.ty()?;

        self.expect(";")?;
        let block = self.block()?;

        Ok(FunctionDeclaration(name, params, block, ty))
    }

    fn formal_parameter_list(&mut self) -> Result<FormalParameterList> {
        traced!(self, "formal_parameter_list");

        let mut list = vec![];
        if let TokenType::Identifier(_) = self.current()? {
            list.push(self.formal_parameter()?);
            while self.consume(";") {
                list.push(self.formal_parameter()?);
            }
        }

        Ok(FormalParameterList(list))
    }

    fn formal_parameter(&mut self) -> Result<FormalParameter> {
        traced!(self, "formal_parameter");

        let mut idents = vec![self.identifier()?];
        while self.consume(",") {
            idents.push(self.identifier()?);
        }
        self.expect(":")?;

        Ok(FormalParameter(idents, self.ty()?))
    }

    fn compound_statement(&mut self) -> Result<Compound> {
        traced!(self, "compound_statement");

        self.expect("begin")?;
        if let TokenType::End = self.current()? {
            self.expect("end")?;
            return Ok(Compound::default());
        }

        let mut statements = vec![];
        while !self.consume("end") {
            if !statements.is_empty() {
                self.expect(";")?;
            }
            statements.push(self.statement()?)
        }
        if statements.is_empty() {
            statements.push(Statement::default())
        }

        Ok(Compound(statements))
    }

    fn statement(&mut self) -> Result<Statement> {
        traced!(self, "statement");

        use self::Statement::*;
        let res = match self.current()? {
            TokenType::Begin => Compound(self.compound_statement()?),
            TokenType::Identifier(_) => match self.peek() {
                Some(TokenType::OpenParen) => FunctionCall(self.function_call()?),
                Some(TokenType::Assign) => Assignment(self.assignment_statement()?),
                _ => self.expected(&[TokenType::OpenParen, TokenType::Assign])?,
            },
            TokenType::If => IfStatement(Box::new(self.if_statement()?)),
            TokenType::Repeat | TokenType::While | TokenType::For => Repetitive(self.repetitive()?),
            TokenType::End | TokenType::SemiColon => Statement::Empty,
            t => self.unexpected(t)?,
        };

        Ok(res)
    }

    fn function_call(&mut self) -> Result<FunctionCall> {
        traced!(self, "function_call");
        trace!("function call");
        let var = self.variable()?;

        self.expect("(")?;
        let params = self.call_params()?;
        self.expect(")")?;

        trace!("var: {:?} | params: {:?}", var, params);

        Ok(FunctionCall(var, params))
    }

    fn assignment_statement(&mut self) -> Result<Assignment> {
        traced!(self, "assignment_statement");

        let var = self.variable()?;
        self.expect(":=")?;
        let expr = self.expression(None)?;

        Ok(Assignment(var, expr))
    }

    fn if_statement(&mut self) -> Result<IfStatement> {
        traced!(self, "if_statement");

        self.expect("if")?;
        let expr = self.expression(None)?;

        self.expect("then")?;
        let statement = self.statement()?;

        let if_ = if self.consume("else") {
            if let Ok(TokenType::If) = self.current() {
                // don't eat the if
                IfStatement::IfElseIf(expr, statement, Box::new(self.if_statement()?))
            } else {
                IfStatement::IfElse(expr, statement, self.statement()?)
            }
        } else {
            IfStatement::If(expr, statement)
        };

        Ok(if_)
    }

    fn repetitive(&mut self) -> Result<Repetitive> {
        traced!(self, "repetitive");

        match self.current()? {
            TokenType::Repeat => {
                self.tokens.advance();
                let mut statements = vec![self.statement()?];
                while !self.consume("until") {
                    self.expect(";")?;
                    statements.push(self.statement()?);
                }
                // TODO check to see if this is boolean
                let expr = self.expression(None)?;
                Ok(Repetitive::Repeat(statements, expr))
            }
            TokenType::While => {
                self.tokens.advance();
                // TODO check to see if this is boolean
                let expr = self.expression(None)?;
                self.expect("do")?;
                let comp = self.compound_statement()?;
                Ok(Repetitive::While(expr, comp))
            }
            TokenType::For => {
                self.tokens.advance();
                let var = self.variable()?;
                self.expect(":=")?;
                let start = self.expression(None)?;
                let dir = match self.current()? {
                    TokenType::To => Direction::To,
                    TokenType::Downto => Direction::DownTo,
                    _ => self.expected(&[TokenType::To, TokenType::Downto])?,
                };
                self.advance();
                let end = self.expression(None)?;
                self.expect("do")?;
                let comp = self.compound_statement()?;
                Ok(Repetitive::For(var, start, dir, end, comp))
            }
            e => unreachable!("repetitive token: {:?}", e),
        }
    }

    fn call_params(&mut self) -> Result<CallParams> {
        traced!(self, "call_params");
        trace!("call_params: {:?}", self.current()?);

        if let TokenType::CloseParen = self.current()? {
            return Ok(CallParams::default());
        }

        let mut params = vec![self.expression(None)?];
        while self.consume(",") {
            params.push(self.expression(None)?)
        }

        Ok(CallParams(params))
    }

    fn grouping(&mut self) -> Result<GroupExpression> {
        traced!(self, "grouping");

        self.expect("(")?;
        let expr = self.expression(None)?;
        self.expect(")")?;

        Ok(GroupExpression(expr))
    }

    fn expression(&mut self, p: Option<Precedence>) -> Result<Expression> {
        traced!(self, "expression");

        let current = &self.current()?;
        let mut lhs = self.prefix(current)?;
        let p = p.unwrap_or(Precedence::None);

        let next = |token| {
            use self::TokenType::*;
            use self::Precedence::*;
            match token {
                Plus | Minus => BinaryAdd,
                Mul | IntDiv | Div => BinaryMul,
                And | Or => BinaryBool,
                LessThan | GreaterThan => Relative,
                LessThanEqual | GreaterThanEqual => Relative,
                Equal | NotEqual => Relative,
                OpenParen => Call,
                _ => None,
            }
        };

        while p < next(self.current()?) {
            let token = self.current()?;
            lhs = self.infix(&token, lhs)?;
        }

        Ok(lhs)
    }

    fn prefix(&mut self, tok: &TokenType) -> Result<Expression> {
        traced!(self, "prefix");

        use self::TokenType::*;
        let ok = match tok {
            // literal
            Integer(_) | Real(_) | String(_) | True | False => Expression::Literal(self.literal()?),
            // unary literal
            Plus | Minus => Expression::Unary(Box::new(self.unary_op(Precedence::UnaryLiteral)?)),
            // unary bool
            Not => Expression::Unary(Box::new(self.unary_op(Precedence::UnaryBool)?)),
            // grouping
            OpenParen => Expression::Group(Box::new(self.grouping()?)),
            // variable
            Identifier(_) => Expression::Variable(self.variable()?),
            _ => self.expected(&[
                Integer(0),
                Real(0.),
                String("".into()),
                Plus,
                Minus,
                Not,
                OpenParen,
                Identifier("".into()),
            ])?,
        };

        Ok(ok)
    }

    fn infix(&mut self, tok: &TokenType, lhs: Expression) -> Result<Expression> {
        traced!(self, "infix");

        use self::TokenType::*;
        let ok = match tok {
            // binary add
            Plus | Minus => {
                Expression::Binary(Box::new(self.binary_op(lhs, Precedence::BinaryAdd)?))
            }
            // binary mul
            Mul | IntDiv | Div => {
                Expression::Binary(Box::new(self.binary_op(lhs, Precedence::BinaryMul)?))
            }
            // binary bool
            And | Or => Expression::Boolean(Box::new(self.binary_op(lhs, Precedence::BinaryBool)?)),
            // relative
            LessThan | GreaterThan => {
                Expression::Boolean(Box::new(self.binary_op(lhs, Precedence::Relative)?))
            }
            // relative
            LessThanEqual | GreaterThanEqual => {
                Expression::Boolean(Box::new(self.binary_op(lhs, Precedence::Relative)?))
            }
            // relative
            Equal | NotEqual => {
                Expression::Boolean(Box::new(self.binary_op(lhs, Precedence::Relative)?))
            }
            // call
            OpenParen => Expression::FunctionCall(self.function_call_expr(lhs)?),
            _ => self.expected(&[
                Plus,
                Minus,
                Mul,
                IntDiv,
                Div,
                And,
                Or,
                LessThan,
                GreaterThan,
                LessThanEqual,
                GreaterThanEqual,
                Equal,
                NotEqual,
                OpenParen,
            ])?,
        };

        Ok(ok)
    }

    fn unary_op(&mut self, p: Precedence) -> Result<UnaryExpression> {
        traced!(self, "unary_op");

        let op = self.current()?.as_unary_op().unwrap();
        self.advance();

        Ok(UnaryExpression(op, self.expression(Some(p))?))
    }

    fn binary_op(&mut self, lhs: Expression, p: Precedence) -> Result<BinaryExpression> {
        traced!(self, "binary_op");

        let op = self.current()?.as_binary_op().unwrap();
        self.advance();

        Ok(BinaryExpression(lhs, op, self.expression(Some(p))?))
    }

    fn function_call_expr(&mut self, lhs: Expression) -> Result<FunctionCall> {
        traced!(self, "function_call_expr");

        let name = match lhs {
            Expression::Variable(name) => name,
            _ => self.error("variable expression required for function_call_expr")?,
        };

        let params = if let TokenType::OpenParen = self.current()? {
            self.advance();
            self.call_params()?
        } else {
            CallParams::default()
        };

        Ok(FunctionCall(name.clone(), params))
    }

    fn literal(&mut self) -> Result<Literal> {
        traced!(self, "literal");

        let res = match self.current()? {
            TokenType::Integer(n) => Literal::Integer(n),
            TokenType::Real(n) => Literal::Real(n),
            TokenType::True => Literal::Boolean(true),
            TokenType::False => Literal::Boolean(false),
            TokenType::String(n) => Literal::String(n),
            _ => self.expected(&[
                TokenType::Integer(0),
                TokenType::Real(0.),
                TokenType::True,
                TokenType::False,
                TokenType::String("".into()),
            ])?,
        };
        self.advance();

        Ok(res)
    }

    fn variable(&mut self) -> Result<Variable> {
        traced!(self, "variable");

        Ok(Variable(self.identifier()?))
    }

    fn ty(&mut self) -> Result<Type> {
        traced!(self, "ty");

        match self.expect(TokenType::TypeName(token::Type::Unit))? {
            TokenType::TypeName(ty) => Ok(ty.into()),
            _ => unreachable!(),
        }
    }

    fn identifier(&mut self) -> Result<String> {
        traced!(self, "identifier");

        match self.expect(TokenType::Identifier("".into()))? {
            TokenType::Identifier(id) => Ok(id),
            _ => unreachable!(),
        }
    }

    fn consume<T: Into<TokenType>>(&mut self, tok: T) -> bool {
        traced!(self, "consume");

        let tok = tok.into();
        match self.current() {
            Ok(ref t) if *t == tok => {
                self.tokens.advance();
                true
            }
            _ => false,
        }
    }

    fn peek(&mut self) -> Option<TokenType> {
        traced!(self, "peek");

        self.tokens.peek()
    }

    fn current(&mut self) -> Result<TokenType> {
        traced!(self, "current");

        match self.tokens.current().take() {
            Some(t) => Ok(t),
            None => self.unexpected(TokenType::EOF),
        }
    }

    fn advance(&mut self) {
        traced!(self, "advance");

        self.tokens.advance()
    }

    fn expect<T: Into<TokenType> + Clone>(&mut self, tok: T) -> Result<TokenType> {
        traced!(self, "expect");

        use std::mem::discriminant;
        let current = self.current()?;
        let tok = tok.into();
        if discriminant(&current) == discriminant(&tok) {
            self.tokens.advance();
            return Ok(current);
        }

        self.expected(&[tok])
    }

    fn expected<E: Into<TokenType> + Clone, T>(&mut self, tok: &[E]) -> Result<T> {
        traced!(self, "expected");

        let current = self.current()?;
        self.error(ErrorKind::Expected(
            tok.iter().map(|s| s.clone().into()).collect::<Vec<_>>(),
            current,
        ))
    }

    fn unexpected<E: Into<TokenType>, T>(&self, tok: E) -> Result<T> {
        traced!(self, "unexpected");

        self.error(ErrorKind::Unexpected(tok.into()))
    }

    fn error<E: Into<ErrorKind>, T>(&self, err: E) -> Result<T> {
        traced!(self, "error");

        let error = Error::new(
            err,
            self.tokens.previous_span(),
            self.source.clone(),
            self.filename.clone(),
        );
        Err(error)
    }
}

impl TokenType {
    fn as_unary_op(&self) -> Option<UnaryOperator> {
        let op = match self {
            TokenType::Plus => UnaryOperator::Plus,
            TokenType::Minus => UnaryOperator::Minus,
            TokenType::Not => UnaryOperator::Not,
            _ => return None,
        };
        Some(op)
    }

    fn as_binary_op(&self) -> Option<BinaryOperator> {
        let res = match self {
            TokenType::Plus => BinaryOperator::Plus,
            TokenType::Minus => BinaryOperator::Minus,
            TokenType::Mul => BinaryOperator::Mul,
            TokenType::Div => BinaryOperator::Div,
            TokenType::IntDiv => BinaryOperator::IntDiv,

            TokenType::LessThan => BinaryOperator::LessThan,
            TokenType::GreaterThan => BinaryOperator::GreaterThan,
            TokenType::LessThanEqual => BinaryOperator::LessThanEqual,
            TokenType::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
            TokenType::Equal => BinaryOperator::Equal,
            TokenType::NotEqual => BinaryOperator::NotEqual,

            TokenType::And => BinaryOperator::And,
            TokenType::Or => BinaryOperator::Or,
            _ => return None,
        };

        Some(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_eq {
        ($left:expr, $right:expr,) => {
            assert_eq!($left, $right)
        };
        ($left:expr, $right:expr) => {{
            #[cfg(feature = "dump")]
            {
                ::std::assert_eq!($left, $right);
                return;
            }

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
        }};
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
        let input = r#"
        var a: integer;
        function b(): string; begin end 
        procedure c; begin end
        "#;

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
    fn for_statement() {
        let input = r#"
         for x := 0 to 10 do
         begin
             y := x + 1
         end;
         "#;

        let mut parser = make_parser(input);
        assert_eq!(
            parser.statement(),
            Ok(Statement::Repetitive(Repetitive::For(
                Variable("x".into()),
                Expression::Literal(Literal::Integer(0)),
                Direction::To,
                Expression::Literal(Literal::Integer(10)),
                Compound(vec![Statement::Assignment(Assignment(
                    Variable("y".into()),
                    Expression::Binary(Box::new(BinaryExpression(
                        Expression::Variable(Variable("x".into())),
                        BinaryOperator::Plus,
                        Expression::Literal(Literal::Integer(1)),
                    ))),
                ))]),
            )))
        );

        let input = r#"
         for x := 10 downto 0 do
         begin
             y := x + 1
         end;
         "#;

        let mut parser = make_parser(input);
        assert_eq!(
            parser.statement(),
            Ok(Statement::Repetitive(Repetitive::For(
                Variable("x".into()),
                Expression::Literal(Literal::Integer(10)),
                Direction::DownTo,
                Expression::Literal(Literal::Integer(0)),
                Compound(vec![Statement::Assignment(Assignment(
                    Variable("y".into()),
                    Expression::Binary(Box::new(BinaryExpression(
                        Expression::Variable(Variable("x".into())),
                        BinaryOperator::Plus,
                        Expression::Literal(Literal::Integer(1)),
                    ))),
                ))]),
            )))
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

    fn print_expr(expr: &Expression, buf: &mut String) {
        match expr {
            Expression::Group(g) => {
                let GroupExpression(expr) = &**g;
                buf.push_str("(");
                print_expr(&expr, buf);
                buf.push_str(")");
            }
            Expression::Binary(b) => {
                let BinaryExpression(lhs, op, rhs) = &**b;
                print_expr(&lhs, buf);
                buf.push_str(&format!(" {} ", op));
                print_expr(&rhs, buf);
            }
            Expression::Literal(Literal::Integer(n)) => buf.push_str(&format!("{}", n)),
            _ => {}
        }
    }

    #[test]
    fn expr_grouped() {
        let input = "(5 + 5)";
        let mut parser = make_parser(input);
        let expr = parser.expression(None).unwrap();
        let mut buf = String::new();
        print_expr(&expr, &mut buf);
        debug!("{}", buf);

        assert_eq!(
            expr,
            Expression::Group(Box::new(GroupExpression(Expression::Binary(Box::new(
                BinaryExpression(
                    Expression::Literal(Literal::Integer(5)),
                    BinaryOperator::Plus,
                    Expression::Literal(Literal::Integer(5)),
                )
            )))))
        );

        let input = "(5 + (5 - 5))";
        let mut parser = make_parser(input);
        let expr = parser.expression(None).unwrap();
        let mut buf = String::new();
        print_expr(&expr, &mut buf);
        debug!("{}", buf);

        assert_eq!(
            expr,
            Expression::Group(Box::new(GroupExpression(Expression::Binary(Box::new(
                BinaryExpression(
                    Expression::Literal(Literal::Integer(5)),
                    BinaryOperator::Plus,
                    Expression::Group(Box::new(GroupExpression(Expression::Binary(Box::new(
                        BinaryExpression(
                            Expression::Literal(Literal::Integer(5)),
                            BinaryOperator::Minus,
                            Expression::Literal(Literal::Integer(5))
                        )
                    )))))
                )
            )))))
        );

        let input = "((1 + 2) - 3)";
        let mut parser = make_parser(input);
        let expr = parser.expression(None).unwrap();
        let mut buf = String::new();
        print_expr(&expr, &mut buf);
        debug!("{}", buf);
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
                BinaryOperator::IntDiv,
                Expression::Literal(Literal::Integer(5)),
            ))))
        )
    }

    #[test]
    fn expr_binary_real_div() {
        let input = "5 / 5";
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
    fn expr_binary_and() {
        let input = "5 and 6";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::And,
                Expression::Literal(Literal::Integer(6)),
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
        let input = "5 = 10";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Boolean(Box::new(BinaryExpression(
                Expression::Literal(Literal::Integer(5)),
                BinaryOperator::Equal,
                Expression::Literal(Literal::Integer(10)),
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

        let input = "begin begin end; begin end end";
        let mut parser = make_parser(input);
        assert_eq!(
            parser.compound_statement(),
            Ok(Compound(vec![
                Statement::Compound(Compound::default()),
                Statement::Compound(Compound::default())
            ]))
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

    fn make_parser(input: &str) -> Parser {
        #[cfg(feature = "dump")]
        let _ = env_logger::Builder::from_default_env()
            .default_format_timestamp(false)
            .try_init();

        #[cfg(feature = "dump")]
        enable_colors();

        #[cfg(feature = "dump")]
        enable_tracer();

        #[cfg(feature = "dump")]
        eprintln!("{}", &input);

        let tokens = scan(&input);
        Parser::new(tokens, input.to_string(), "".to_string())
    }
}
