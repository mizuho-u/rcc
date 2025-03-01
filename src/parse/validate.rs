use std::cell::RefCell;

use super::{
    env::Env, label::validate_label, Block, BlockItem, Declaration, Expression, Function,
    Identifier, Program, Statement, UnaryOperator,
};

#[derive(Debug)]
pub struct SemanticError(pub String);

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for SemanticError {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub fn validate(p: Program) -> Result<Program, SemanticError> {
    let mut var = Env::new();
    let mut label = Env::new();

    let Program::Program(Function::Function(id, Block::Block(body))) = p;

    let mut block = resolve_block(Block::Block(body), &mut var, &mut label)?;

    validate_label(&mut block, &label)?;

    Ok(Program::Program(Function::Function(id, block)))
}

fn resolve_block(b: Block, env: &mut Env, labelmap: &mut Env) -> Result<Block, SemanticError> {
    let Block::Block(body) = b;

    let mut r_body = Vec::new();
    for b in body {
        r_body.push(resolve_block_item(b, env, labelmap)?);
    }

    Ok(Block::Block(r_body))
}

fn resolve_block_item(
    b: BlockItem,
    varenv: &mut Env,
    labelenv: &mut Env,
) -> Result<BlockItem, SemanticError> {
    match b {
        BlockItem::Statement(s) => Ok(BlockItem::Statement(resolve_statement(
            s, varenv, labelenv,
        )?)),
        BlockItem::Declaration(declaration) => match declaration {
            Declaration::Declaration(Identifier(s), exp) => {
                if varenv.contains_current(&s) {
                    return Err(SemanticError("Duplicate variable declaration".to_string()));
                }

                let name = make_temporary(&format!("var.{}", &s));
                varenv.set(s.clone(), name.clone());

                let exp = if let Some(exp) = exp {
                    Some(resolve_exp(exp, varenv)?)
                } else {
                    None
                };

                Ok(BlockItem::Declaration(Declaration::Declaration(
                    Identifier(name),
                    exp,
                )))
            }
        },
    }
}

fn resolve_statement(
    s: Statement,
    varenv: &mut Env,
    labelenv: &mut Env,
) -> Result<Statement, SemanticError> {
    match s {
        Statement::Return(e) => Ok(Statement::Return(resolve_exp(e, varenv)?)),
        Statement::Expression(e) => Ok(Statement::Expression(resolve_exp(e, varenv)?)),
        Statement::Null => Ok(Statement::Null),
        Statement::If(cond, then, el) => {
            let cond = resolve_exp(cond, varenv)?;
            let then = resolve_statement(*then, &mut varenv.extend(), labelenv)?;
            if let Some(el) = el {
                let el = resolve_statement(*el, &mut varenv.extend(), labelenv)?;
                Ok(Statement::If(cond, Box::new(then), Some(Box::new(el))))
            } else {
                Ok(Statement::If(cond, Box::new(then), None))
            }
        }
        Statement::Goto(id) => Ok(Statement::Goto(id)),
        Statement::Label(id, ls) => {
            if labelenv.contains(&id.0) {
                return Err(SemanticError(format!("duplicated label {}", &id.0)));
            }

            if let Some(s) = labelenv.get(&id.0) {
                Ok(Statement::Label(Identifier(s.clone()), ls))
            } else {
                let name = make_temporary(&format!("lbl.{}", &id.0));
                labelenv.set(id.0, name.clone());

                Ok(Statement::Label(
                    Identifier(name),
                    Box::new(resolve_statement(*ls, varenv, labelenv)?),
                ))
            }
        }
        Statement::Compound(b) => Ok(Statement::Compound(resolve_block(
            b,
            &mut varenv.extend(),
            labelenv,
        )?)),
    }
}

fn resolve_exp(exp: Expression, varenv: &Env) -> Result<Expression, SemanticError> {
    match exp {
        Expression::Var(Identifier(s)) => {
            if let Some(v) = varenv.get(&s) {
                Ok(Expression::Var(Identifier(v.clone())))
            } else {
                Err(SemanticError("Undeclared variable".to_string()))
            }
        }
        Expression::Assignment(op, e1, e2) => match *e1 {
            Expression::Var(_) => Ok(Expression::Assignment(
                op,
                Box::new(resolve_exp(*e1, varenv)?),
                Box::new(resolve_exp(*e2, varenv)?),
            )),
            _ => Err(SemanticError("Invalid lvalue".to_string())),
        },
        Expression::Unary(op, e)
            if op == UnaryOperator::IncrementPrefix
                || op == UnaryOperator::IncrementPostfix
                || op == UnaryOperator::DecrementPrefix
                || op == UnaryOperator::DecrementPostfix =>
        {
            // 識別子のみに適用できる
            match *e {
                Expression::Var(_) => Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varenv)?))),
                Expression::Unary(UnaryOperator::Complement, _) => {
                    Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varenv)?)))
                }
                Expression::Unary(UnaryOperator::Not, _) => {
                    Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varenv)?)))
                }
                Expression::Unary(UnaryOperator::Negate, _) => {
                    Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varenv)?)))
                }
                _ => Err(SemanticError("Invalid lvalue".to_string())),
            }
        }
        Expression::Unary(op, e) => Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varenv)?))),
        Expression::Binary(op, e1, e2) => Ok(Expression::Binary(
            op,
            Box::new(resolve_exp(*e1, varenv)?),
            Box::new(resolve_exp(*e2, varenv)?),
        )),
        Expression::Conditional(cond, e1, e2) => Ok(Expression::Conditional(
            Box::new(resolve_exp(*cond, varenv)?),
            Box::new(resolve_exp(*e1, varenv)?),
            Box::new(resolve_exp(*e2, varenv)?),
        )),
        _ => Ok(exp),
    }
}

fn make_temporary(prefix: &String) -> String {
    thread_local!(
        pub static LABEL_COUNT: RefCell<i32> = RefCell::new(0)
    );

    let count = LABEL_COUNT.with(|c| {
        let mut c = c.borrow_mut();
        *c += 1;

        *c
    });

    format!("{}.{}", prefix, count)
}
