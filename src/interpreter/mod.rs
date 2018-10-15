use crate::prelude::ast::*;

mod builtin;
mod error;
mod object;
mod primitive;
mod scope;

use self::error::{Error, OperatorError};
use self::object::{Builtin, Object};
use self::primitive::Primitive;
use self::scope::Scope;

type Result<T> = ::std::result::Result<T, Error>;

#[derive(Default)]
pub struct Interpreter {
    scope: Option<Scope>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn evaluate(&mut self, program: Program) {
        if let Err(err) = self.visit(program) {
            error!("{}", err);
            error!("{:#?}", self.scope);
            ::std::process::exit(1)
        }
    }

    fn visit(&mut self, node: Program) -> Result<()> {
        let Program(Variable(name), block) = node;
        self.enter(name);
        self.init()?;
        info!("result: {:#?}", self.visit_block(block)?);
        self.leave();
        Ok(())
    }

    fn visit_block(&mut self, node: Block) -> Result<Object> {
        let Block(decls, comp) = node;
        self.visit_declarations(&decls)?;
        self.visit_compound(comp)
    }

    fn visit_declarations(&mut self, node: &[Declaration]) -> Result<()> {
        for decl in node {
            match decl {
                Declaration::Procedure(list) => {
                    for proc in list {
                        self.visit_procedure_declaration(proc.clone())?;
                    }
                }
                Declaration::Function(list) => {
                    for func in list {
                        self.visit_function_declaration(func.clone())?;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn visit_procedure_declaration(&mut self, node: ProcedureDeclaration) -> Result<()> {
        let ProcedureDeclaration(name, list, block) = node;
        let params = self.visit_formal_parameter_list(list)?;
        self.scope()?
            .set(name.clone(), Object::Procedure(name, params, block));
        Ok(())
    }

    fn visit_function_declaration(&mut self, node: FunctionDeclaration) -> Result<()> {
        let FunctionDeclaration(name, list, block, ty) = node;
        let params = self.visit_formal_parameter_list(list)?;
        self.scope()?
            .set(name.clone(), Object::Function(name, params, block, ty));
        Ok(())
    }

    fn visit_formal_parameter_list(&mut self, node: FormalParameterList) -> Result<Vec<String>> {
        let FormalParameterList(list) = node;
        let mut names = vec![];
        for param in list {
            let mut new = self.visit_formal_parameter(param)?;
            names.append(&mut new);
        }
        Ok(names)
    }

    fn visit_formal_parameter(&mut self, node: FormalParameter) -> Result<Vec<String>> {
        let FormalParameter(names, _) = node;
        Ok(names)
    }

    fn visit_compound(&mut self, node: Compound) -> Result<Object> {
        let Compound(list) = node;
        let mut obj = Object::Unit;
        for statement in list {
            obj = self.visit_statement(statement)?;
        }
        Ok(obj)
    }

    fn visit_statement(&mut self, node: Statement) -> Result<Object> {
        match node {
            Statement::Compound(comp) => self.visit_compound(comp),
            Statement::Assignment(assign) => self.visit_assignment(assign),
            Statement::FunctionCall(call) => self.visit_function_call(call),
            Statement::IfStatement(stmt) => self.visit_if_statement(*stmt),
            Statement::Empty => Ok(Object::Unit),
        }
    }

    fn visit_function_call(&mut self, node: FunctionCall) -> Result<Object> {
        let FunctionCall(Variable(func), CallParams(params)) = node;
        match self.scope()?.get(func.clone()).cloned() {
            Some(Object::Function(name, args, block, _))
            | Some(Object::Procedure(name, args, block)) => {
                self.enter(name.clone());
                for (a, p) in args.iter().zip(params.iter()) {
                    let p = self.visit_expression(p.clone())?;
                    self.scope()?.set(a.to_string(), p);
                }
                let result = self.visit_block(block)?;
                self.leave();
                Ok(result)
            }

            Some(Object::Builtin(Builtin::Write(f)))
            | Some(Object::Builtin(Builtin::WriteLn(f))) => {
                match self.visit_expression(params[0].clone())? {
                    o @ Object::Primitive(Primitive::Integer(_))
                    | o @ Object::Primitive(Primitive::String(_))
                    | o @ Object::Primitive(Primitive::Boolean(_))
                    | o @ Object::Primitive(Primitive::Real(_)) => f(o),

                    o => {
                        warn!("invalid argument: {:#?}", o);
                        Err(Error::InvalidArgument)
                    }
                }
            }

            Some(Object::Builtin(Builtin::ReadLn(f))) => f(),
            _ => Err(Error::UnknownFunction(func.to_string()))?,
        }
    }

    fn visit_group(&mut self, node: GroupExpression) -> Result<Object> {
        let GroupExpression(expr) = node;
        self.visit_expression(expr)
    }

    fn visit_if_statement(&mut self, node: IfStatement) -> Result<Object> {
        match node {
            IfStatement::If(expr, stmt) => match self.visit_expression(expr)? {
                Object::Primitive(Primitive::Boolean(true)) => {
                    self.visit_statement(stmt)?;
                    Ok(Object::Unit)
                }
                Object::Primitive(Primitive::Boolean(false)) => Ok(Object::Unit),
                _ => panic!(),
            },

            IfStatement::IfElse(expr, if_, else_) => match self.visit_expression(expr)? {
                Object::Primitive(Primitive::Boolean(true)) => {
                    self.visit_statement(if_)?;
                    Ok(Object::Unit)
                }
                Object::Primitive(Primitive::Boolean(false)) => {
                    self.visit_statement(else_)?;
                    Ok(Object::Unit)
                }
                _ => panic!(),
            },

            IfStatement::IfElseIf(expr, if_, else_) => match self.visit_expression(expr)? {
                Object::Primitive(Primitive::Boolean(true)) => {
                    self.visit_statement(if_)?;
                    Ok(Object::Unit)
                }
                Object::Primitive(Primitive::Boolean(false)) => {
                    self.visit_if_statement(*else_)?;
                    Ok(Object::Unit)
                }
                _ => panic!(),
            },
        }
    }

    fn visit_assignment(&mut self, node: Assignment) -> Result<Object> {
        let Assignment(Variable(name), expr) = node;
        let val = self.visit_expression(expr)?;
        self.scope()?.set(name, val.clone());
        Ok(val)
    }

    fn visit_expression(&mut self, node: Expression) -> Result<Object> {
        match node {
            Expression::Unary(expr) => self.visit_unary_op(*expr),
            Expression::Binary(expr) => self.visit_binary_op(*expr),
            Expression::Literal(expr) => self.visit_literal(expr),
            Expression::Variable(expr) => self.visit_variable(expr),
            Expression::FunctionCall(call) => self.visit_function_call(call),
            Expression::Group(expr) => self.visit_group(*expr),
        }
    }

    fn visit_unary_op(&mut self, node: UnaryExpression) -> Result<Object> {
        match node {
            UnaryExpression(UnaryOperator::Plus, expr) => self.visit_expression(expr)?.unary_plus(),
            UnaryExpression(UnaryOperator::Minus, expr) => {
                self.visit_expression(expr)?.unary_minus()
            }
            UnaryExpression(UnaryOperator::Not, expr) => self.visit_expression(expr)?.negate(),
        }
    }

    fn visit_binary_op(&mut self, node: BinaryExpression) -> Result<Object> {
        use crate::prelude::ast::{BinaryExpression as Ex, BinaryOperator as Op};
        let mut visit = |e| self.visit_expression(e);
        match node {
            Ex(left, Op::Plus, expr) => visit(left)?.add(&visit(expr)?),
            Ex(left, Op::Minus, expr) => visit(left)?.subtract(&visit(expr)?),
            Ex(left, Op::Mul, expr) => visit(left)?.multiply(&visit(expr)?),
            Ex(left, Op::Div, expr) => visit(left)?.int_divide(&visit(expr)?),

            Ex(left, Op::RealDiv, expr) => visit(left)?.real_divide(&visit(expr)?),

            Ex(left, Op::And, expr) => visit(left)?.and(&visit(expr)?),
            Ex(left, Op::Or, expr) => visit(left)?.or(&visit(expr)?),
            Ex(left, Op::LessThan, expr) => visit(left)?.less_than(&visit(expr)?),
            Ex(left, Op::GreaterThan, expr) => visit(left)?.greater_than(&visit(expr)?),
            Ex(left, Op::LessThanEqual, expr) => visit(left)?.less_than_equal(&visit(expr)?),
            Ex(left, Op::GreaterThanEqual, expr) => visit(left)?.greater_than_equal(&visit(expr)?),
            Ex(left, Op::Equal, expr) => visit(left)?.equal(&visit(expr)?),
            Ex(left, Op::NotEqual, expr) => visit(left)?.not_equal(&visit(expr)?),
        }
    }

    fn visit_literal(&mut self, node: Literal) -> Result<Object> {
        Ok(match node {
            Literal::Integer(n) => n.into(),
            Literal::String(s) => s.into(),
            Literal::Boolean(b) => b.into(),
            Literal::Real(r) => r.into(),
        })
    }

    fn visit_variable(&mut self, node: Variable) -> Result<Object> {
        let Variable(name) = node;
        self.scope()?
            .get(&name)
            .cloned()
            .ok_or_else(|| Error::UnknownVariable(name))
    }

    fn init(&mut self) -> Result<()> {
        let scope = self.scope()?;
        scope.set("write", Object::Builtin(Builtin::Write(builtin::write)));
        scope.set(
            "writeln",
            Object::Builtin(Builtin::WriteLn(builtin::writeln)),
        );
        scope.set("readln", Object::Builtin(Builtin::ReadLn(builtin::readln)));
        Ok(())
    }

    fn enter(&mut self, name: impl Into<String>) {
        self.scope = match self.scope.take() {
            Some(scope) => Some(Scope::with_parent(name, scope)),
            None => Some(Scope::new(name.into())),
        }
    }

    fn leave(&mut self) {
        self.scope = self.scope.take().and_then(|s| s.parent())
    }

    fn scope(&mut self) -> Result<&mut Scope> {
        self.scope.as_mut().ok_or_else(|| Error::UnknownScope)
    }
}
