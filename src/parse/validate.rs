use std::cell::RefCell;

use super::{
    env::Env, goto_label::validate_label, Block, BlockItem, Declaration, Expression, Function,
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

pub fn validate(p: &mut Program) -> Result<(), SemanticError> {
    let mut var = Env::new();
    let mut label = Env::new();

    let Program::Program(Function::Function(_id, b)) = p;

    resolve_block(b, &mut var, &mut label)?;

    validate_label(b, &label)?;

    Ok(())
}

fn resolve_block(b: &mut Block, env: &mut Env, labelmap: &mut Env) -> Result<(), SemanticError> {
    let Block::Block(body) = b;

    for b in body {
        resolve_block_item(b, env, labelmap)?;
    }

    Ok(())
}

fn resolve_block_item(
    b: &mut BlockItem,
    varenv: &mut Env,
    labelenv: &mut Env,
) -> Result<(), SemanticError> {
    match b {
        BlockItem::Statement(s) => resolve_statement(s, varenv, labelenv)?,
        BlockItem::Declaration(declaration) => match declaration {
            Declaration::Declaration(Identifier(s), exp) => {
                if varenv.contains_current(&s) {
                    return Err(SemanticError("Duplicate variable declaration".to_string()));
                }

                let name = make_temporary(&format!("var.{}", &s));
                varenv.set(s.clone(), name.clone());

                *s = name;

                if let Some(exp) = exp {
                    resolve_exp(exp, varenv)?;
                };
            }
        },
    };

    Ok(())
}

fn resolve_statement(
    s: &mut Statement,
    varenv: &mut Env,
    labelenv: &mut Env,
) -> Result<(), SemanticError> {
    match s {
        Statement::Return(e) => resolve_exp(e, varenv)?,
        Statement::Expression(e) => resolve_exp(e, varenv)?,
        Statement::If(cond, then, el) => {
            resolve_exp(cond, varenv)?;
            resolve_statement(then, &mut varenv.extend(), labelenv)?;
            if let Some(el) = el {
                resolve_statement(el, &mut varenv.extend(), labelenv)?;
            };
        }
        Statement::Label(id, ls) => {
            if labelenv.contains(&id.0) {
                return Err(SemanticError(format!("duplicated label {}", &id.0)));
            }

            if let Some(s) = labelenv.get(&id.0) {
                *id = Identifier(s.clone());
            } else {
                let name = make_temporary(&format!("lbl.{}", &id.0));
                labelenv.set(id.0.clone(), name.clone());

                *id = Identifier(name);

                resolve_statement(ls, varenv, labelenv)?;
            }
        }
        Statement::Compound(b) => resolve_block(b, &mut varenv.extend(), labelenv)?,
        _ => {}
    }

    Ok(())
}

fn resolve_exp(exp: &mut Expression, varenv: &Env) -> Result<(), SemanticError> {
    match exp {
        Expression::Var(Identifier(s)) => {
            if let Some(v) = varenv.get(&s) {
                *s = v.to_string();
            } else {
                return Err(SemanticError("Undeclared variable".to_string()));
            }
        }
        Expression::Assignment(_op, e1, e2) => match e1.as_ref() {
            Expression::Var(_) => {
                resolve_exp(e1, varenv)?;
                resolve_exp(e2, varenv)?;
            }
            _ => return Err(SemanticError("Invalid lvalue".to_string())),
        },
        Expression::Unary(op, e)
            if *op == UnaryOperator::IncrementPrefix
                || *op == UnaryOperator::IncrementPostfix
                || *op == UnaryOperator::DecrementPrefix
                || *op == UnaryOperator::DecrementPostfix =>
        {
            // 識別子のみに適用できる
            match e.as_ref() {
                Expression::Var(_) => resolve_exp(e, varenv)?,
                Expression::Unary(UnaryOperator::Complement, _) => resolve_exp(e, varenv)?,
                Expression::Unary(UnaryOperator::Not, _) => resolve_exp(e, varenv)?,
                Expression::Unary(UnaryOperator::Negate, _) => resolve_exp(e, varenv)?,
                _ => return Err(SemanticError("Invalid lvalue".to_string())),
            }
        }
        Expression::Unary(_op, e) => resolve_exp(e, varenv)?,
        Expression::Binary(_op, e1, e2) => {
            resolve_exp(e1, varenv)?;
            resolve_exp(e2, varenv)?;
        }
        Expression::Conditional(cond, e1, e2) => {
            resolve_exp(cond, varenv)?;
            resolve_exp(e1, varenv)?;
            resolve_exp(e2, varenv)?;
        }
        _ => {}
    };

    Ok(())
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
