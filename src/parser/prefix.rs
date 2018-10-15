use super::*;

#[derive(Debug)]
pub enum PrefixParser {
    Literal,
    Variable,
    UnaryOperator(Precedence),
    Grouping,
}

impl PrefixParser {
    pub fn parse(&self, parser: &mut Parser) -> Result<Expression> {
        Ok(match self {
            PrefixParser::Literal => Expression::Literal(parser.literal()?),
            PrefixParser::Variable => Expression::Variable(parser.variable_expr()?),
            PrefixParser::UnaryOperator(_) => Expression::Unary(Box::new(parser.unary_op()?)),
            PrefixParser::Grouping => Expression::Group(Box::new(parser.grouping()?)),
        })
    }

    #[allow(dead_code)]
    pub fn precedence(&self) -> u32 {
        if let PrefixParser::UnaryOperator(p) = *self {
            return p as u32;
        }
        0
    }
}
