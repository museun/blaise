use super::*;

pub struct Parser {
    pub(crate) tokens: Tokens,
    source: String,
    filename: String,
}

impl Parser {
    pub fn new(tokens: Tokens, source: String, filename: String) -> Self {
        Self {
            tokens,
            source,
            filename,
        }
    }

    pub fn parse(mut self) -> Result<Program> {
        let program = self.program()?;
        let mut tokens = self.tokens;
        match tokens.next_token() {
            Some(TokenType::EOF) | None => Ok(program),
            // unexpected token
            Some(t) => Err(Error::new(
                ErrorKind::Unexpected(t),
                tokens.span(),
                self.source,
                self.filename,
            )),
        }
    }

    pub fn program(&mut self) -> Result<Program> {
        self.expect("program")?;

        let var = self.variable()?;
        self.expect(";")?;

        let block = self.block()?;
        self.expect(".")?;

        Ok(Program(var, block))
    }

    fn block(&mut self) -> Result<Block> {
        let decls = self.declarations()?;
        debug!("decls: {:#?}", decls);
        let compound = self.compound_statement()?;
        Ok(Block(decls, compound))
    }

    fn declarations(&mut self) -> Result<Vec<Declaration>> {
        let (mut decls, mut vars) = (vec![], vec![]);
        if let TokenType::Var = self.current()? {
            self.advance();
            while let TokenType::Identifier(_) = self.current()? {
                vars.push(self.variable_declaration()?)
            }
            if vars.is_empty() {
                panic!("{:?}", self.current()?)
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

                t => {
                    trace!("decl other: current: {:?}", t);
                    break;
                }
            };
        }

        decls.push(Declaration::Variable(vars));
        decls.push(Declaration::Procedure(procs));
        decls.push(Declaration::Function(funcs));

        Ok(decls)
    }

    fn variable_declaration(&mut self) -> Result<VariableDeclaration> {
        // var a,b,c,d : integer;
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
        let mut idents = vec![self.identifier()?];
        while self.consume(",") {
            idents.push(self.identifier()?);
        }
        self.expect(":")?;
        Ok(FormalParameter(idents, self.ty()?))
    }

    fn compound_statement(&mut self) -> Result<Compound> {
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
        use self::Statement::*;
        let res = match self.current()? {
            TokenType::Begin => Compound(self.compound_statement()?),
            TokenType::Identifier(_) => match self.peek() {
                Some(TokenType::OpenParen) => FunctionCall(self.function_call()?),
                Some(TokenType::Assign) => Assignment(self.assignment_statement()?),
                t => panic!("{:?}", t),
            },
            TokenType::If => IfStatement(Box::new(self.if_statement()?)),
            TokenType::Repeat | TokenType::While | TokenType::For => Repetitive(self.repetitive()?),
            TokenType::End | TokenType::SemiColon => Statement::Empty,
            t => panic!("{:?}", t),
        };
        trace!("stmt cur: {:?}", self.current()?);
        Ok(res)
    }

    fn function_call(&mut self) -> Result<FunctionCall> {
        let var = self.variable()?;
        self.expect("(")?;
        let params = self.call_params()?;
        self.expect(")")?;
        Ok(FunctionCall(var, params))
    }

    fn assignment_statement(&mut self) -> Result<Assignment> {
        let var = self.variable()?;
        self.expect(":=")?;
        let expr = self.expression(None)?;
        Ok(Assignment(var, expr))
    }

    fn if_statement(&mut self) -> Result<IfStatement> {
        self.expect("if")?;
        let expr = self.expression(None)?;

        self.expect("then")?;
        let statement = self.statement()?;

        Ok(if self.consume("else") {
            if let Ok(TokenType::If) = self.current() {
                // don't eat the if
                IfStatement::IfElseIf(expr, statement, Box::new(self.if_statement()?))
            } else {
                IfStatement::IfElse(expr, statement, self.statement()?)
            }
        } else {
            IfStatement::If(expr, statement)
        })
    }

    fn repetitive(&mut self) -> Result<Repetitive> {
        match self.current()? {
            TokenType::Repeat => {
                self.tokens.advance();
                // repeat stmt-seq until bool-expr
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
                trace!("expr: {:#?}", expr);
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
                    t => panic!("{:?}", t),
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
        if let TokenType::OpenParen = self.current()? {
            self.advance();
            let expr = self.expression(None)?;
            return match self.current()? {
                TokenType::CloseParen => {
                    self.advance();
                    Ok(GroupExpression(expr))
                }
                t => panic!("{:?}", t),
            };
        };
        panic!("{:?}", self.current()?)
    }

    fn expression(&mut self, p: Option<Precedence>) -> Result<Expression> {
        let mut lhs = self.prefix(&self.current()?)?;
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
        trace!("e expr, lhs: {:?}, cur: {:?}", lhs, self.current()?);
        Ok(lhs)
    }

    fn prefix(&mut self, tok: &TokenType) -> Result<Expression> {
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
            t => panic!("{:?}", t),
        };
        Ok(ok)
    }

    fn infix(&mut self, tok: &TokenType, lhs: Expression) -> Result<Expression> {
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
            t => panic!("{:?}", t),
        };
        Ok(ok)
    }

    fn unary_op(&mut self, p: Precedence) -> Result<UnaryExpression> {
        let tok = self.current()?;
        let op = tok.as_unary_op().unwrap();
        self.advance();
        Ok(UnaryExpression(op, self.expression(Some(p))?))
    }

    fn binary_op(&mut self, lhs: Expression, p: Precedence) -> Result<BinaryExpression> {
        let tok = self.current()?;
        let op = tok.as_binary_op().unwrap();
        self.advance();
        Ok(BinaryExpression(lhs, op, self.expression(Some(p))?))
    }

    fn function_call_expr(&mut self, lhs: Expression) -> Result<FunctionCall> {
        let name = match lhs {
            Expression::Variable(name) => name,
            e => panic!("{:?}", e),
        };

        let params = if let TokenType::OpenParen = self.current()? {
            self.tokens.advance();
            self.call_params()?
        } else {
            CallParams::default()
        };
        Ok(FunctionCall(name.clone(), params))
    }

    fn literal(&mut self) -> Result<Literal> {
        let res = match self.current()? {
            TokenType::Integer(n) => Literal::Integer(n),
            TokenType::Real(n) => Literal::Real(n),
            TokenType::True => Literal::Boolean(true),
            TokenType::False => Literal::Boolean(false),
            TokenType::String(n) => Literal::String(n),
            t => panic!("{:?}", t),
        };
        self.advance();
        Ok(res)
    }

    fn variable(&mut self) -> Result<Variable> {
        let name = self.identifier()?;
        Ok(Variable(name.clone()))
    }

    fn ty(&mut self) -> Result<Type> {
        if let TokenType::TypeName(ty) = self.expect(TokenType::TypeName(token::Type::Unit))? {
            return Ok(ty.into());
        }
        panic!("{:?}", self.current()?)
    }

    fn identifier(&mut self) -> Result<String> {
        if let TokenType::Identifier(id) = self.expect(TokenType::Identifier("".into()))? {
            return Ok(id.clone());
        }
        panic!("{:?}", self.current()?)
    }

    fn consume<T: Into<TokenType>>(&mut self, tok: T) -> bool {
        let tok = tok.into();
        match self.current() {
            Ok(ref t) if *t == tok => {
                trace!("consumed");
                self.tokens.advance();
                true
            }
            _ => false,
        }
    }

    fn peek(&self) -> Option<TokenType> {
        let t = self.tokens.peek();
        trace!("peek: {:?}", t);
        t
    }

    fn current(&self) -> Result<TokenType> {
        self.tokens.current().take().ok_or_else(|| panic!())
    }

    fn advance(&mut self) {
        self.tokens.advance()
    }

    fn next(&mut self) -> Result<TokenType> {
        self.tokens.next_token().take().ok_or_else(|| panic!())
    }

    fn expect<T: Into<TokenType>>(&mut self, tok: T) -> Result<TokenType> {
        use std::mem::discriminant;
        let current = self.current()?;
        let tok = tok.into();
        if discriminant(&current) == discriminant(&tok) {
            self.tokens.advance();
            return Ok(current);
        }
        panic!(
            "{} expected {:?} got {:?}",
            self.tokens.span(),
            tok,
            current
        );
    }

    fn expected_token<T: Into<TokenType>>(&mut self, tok: T) -> Result<()> {
        let tok = tok.into();
        unimplemented!();
    }
}

trait AsOp {
    fn as_unary_op(&self) -> Option<UnaryOperator>;
    fn as_binary_op(&self) -> Option<BinaryOperator>;
}

impl AsOp for TokenType {
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
            if !DEBUG.load(Ordering::Relaxed) {
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
    #[ignore]
    fn nested_grouping() {
        let input = "((1 + 2) - 3)";

        // let expr =
        // Expression::Group(Box::new(GroupExpression(Expression::Binary(Box::new(
        //     BinaryExpression(
        //         Expression::Group(Box::new(GroupExpression(Expression::Binary(Box::
        // new(             BinaryExpression(
        //                 Expression::Literal(Literal::Integer(1)),
        //                 BinaryOperator::Plus,
        //                 Expression::Literal(Literal::Integer(2)),
        //             ),
        //         ))))),
        //         BinaryOperator::Minus,
        //         Expression::Literal(Literal::Integer(3)),
        //     ),
        // )))));
        let mut parser = make_parser(input);
        assert_eq!(
            parser.expression(None),
            Ok(Expression::Literal(Literal::Integer(0)))
        );
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
        assert_eq!(parser.compound_statement(), Ok(Compound::default()));
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

    use std::sync::atomic::{AtomicBool, Ordering};
    static DEBUG: AtomicBool = AtomicBool::new(false);

    fn make_parser(input: &str) -> Parser {
        #[cfg(feature = "dump")]
        DEBUG.store(true, Ordering::Relaxed);

        if DEBUG.load(Ordering::Relaxed) {
            let _ = env_logger::Builder::from_default_env()
                .default_format_timestamp(false)
                .try_init();

            enable_colors();
            enable_tracer();

            eprintln!("{}", &input)
        }

        let tokens = scan(&input);
        Parser::new(tokens, input.to_string(), "".to_string())
    }
}
