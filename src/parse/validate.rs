use std::cell::RefCell;

use super::{
    env::Env, goto_label::resolve_goto_label, loop_label::resolve_loop_label, Block, BlockItem,
    Declaration, Expression, FunctionDeclaration, Identifier, Program, Statement, UnaryOperator,
    VariableDeclaration,
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

    let Program::Program(fs) = p;

    for FunctionDeclaration(_id, _params, b) in fs {
        if let Some(b) = b {
            resolve_variable(b, &mut var, &mut label)?;

            resolve_goto_label(b, &label)?;

            resolve_loop_label(b)?;
        }
    }

    Ok(())
}

fn resolve_variable(b: &mut Block, env: &mut Env, labelmap: &mut Env) -> Result<(), SemanticError> {
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
        BlockItem::Declaration(d) => resolve_declaration(d, varenv)?,
    };

    Ok(())
}

fn resolve_declaration(d: &mut Declaration, varenv: &mut Env) -> Result<(), SemanticError> {
    match d {
        Declaration::Variable(VariableDeclaration(Identifier(s), exp)) => {
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
        Declaration::Function(function_declaration) => todo!(),
    }

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
        Statement::Compound(b) => resolve_variable(b, &mut varenv.extend(), labelenv)?,
        Statement::While(cond, body, _id) => {
            resolve_exp(cond, varenv)?;
            resolve_statement(body, varenv, labelenv)?;
        }
        Statement::DoWhile(body, cond, _id) => {
            resolve_statement(body, varenv, labelenv)?;
            resolve_exp(cond, varenv)?;
        }
        Statement::For(init, cond, post, body, _id) => {
            let mut varenv = varenv.extend();

            match init {
                super::ForInit::Declaration(d) => resolve_declaration(d, &mut varenv)?,
                super::ForInit::Expression(Some(e)) => resolve_exp(e, &varenv)?,
                _ => {}
            };

            if let Some(cond) = cond {
                resolve_exp(cond, &varenv)?;
            };

            if let Some(post) = post {
                resolve_exp(post, &varenv)?;
            };

            resolve_statement(body, &mut varenv, labelenv)?;
        }
        Statement::Switch(ctrl, body, _cases, _id) => {
            resolve_exp(ctrl, varenv)?;
            resolve_statement(body, varenv, labelenv)?;
        }
        Statement::Case(exp, body, _id) => {
            resolve_exp(exp, varenv)?;
            if let Some(body) = body {
                resolve_statement(body, varenv, labelenv)?;
            }
        }
        Statement::Default(body, _id) => {
            if let Some(body) = body {
                resolve_statement(body, varenv, labelenv)?;
            }
        }
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

pub fn make_temporary(prefix: &String) -> String {
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
