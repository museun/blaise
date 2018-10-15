use super::*;

#[derive(Debug)]
pub enum InfixParser {
    BinaryOperator(Precendence),
    FunctionCall(Precendence),
}

impl InfixParser {
    pub fn parse(&self, parser: &mut Parser, left: Expression) -> Result<Expression> {
        Ok(match self {
            InfixParser::BinaryOperator(_) => {
                let op = self.binary_op(parser, left)?;
                Expression::Binary(Box::new(op))
            }

            InfixParser::FunctionCall(_) => {
                Expression::FunctionCall(parser.function_call_expr(left)?)
            }
        })
    }

    pub fn precedence(&self) -> u32 {
        match *self {
            InfixParser::BinaryOperator(p) | InfixParser::FunctionCall(p) => p as u32,
        }
    }

    fn binary_op(&self, parser: &mut Parser, expr: Expression) -> Result<BinaryExpression> {
        let t = parser.tokens.current();
        let op = match t {
            Token::Symbol(s) => s
                .as_binary_op()
                .ok_or_else(|| parser.unexpected::<(), _>(t).unwrap_err())?,

            Token::Reserved(s) => s
                .as_binary_op()
                .ok_or_else(|| parser.unexpected::<(), _>(t).unwrap_err())?,

            t => parser.unexpected(t)?,
        };

        let p = self.precedence();
        Ok(BinaryExpression(expr, op, parser.expression(Some(p))?))
    }
}
