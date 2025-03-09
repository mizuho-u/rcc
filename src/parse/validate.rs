use std::cell::RefCell;

use super::{
    env::{Entry, Env},
    goto_label::resolve_goto_label,
    loop_label::resolve_loop_label,
    Block, BlockItem, Declaration, Expression, FunctionDeclaration, Identifier, Program, Statement,
    UnaryOperator, VariableDeclaration,
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

    let Program::Program(fs) = p;

    for d in fs {
        let label_env = &mut Env::new();
        resolve_function_declaration(d, &mut var, label_env)?;

        if let FunctionDeclaration(_id, _params, Some(body)) = d {
            resolve_goto_label(body, &label_env)?;
            resolve_loop_label(body)?;
        }
    }

    Ok(())
}

fn resolve_identifier(
    b: &mut Block,
    env: &mut Env<Entry>,
    labelmap: &mut Env<String>,
) -> Result<(), SemanticError> {
    resolve_block(b, env, labelmap)
}

fn resolve_block(
    b: &mut Block,
    env: &mut Env<Entry>,
    labelmap: &mut Env<String>,
) -> Result<(), SemanticError> {
    let Block::Block(body) = b;

    for b in body {
        resolve_block_item(b, env, labelmap)?;
    }

    Ok(())
}

fn resolve_block_item(
    b: &mut BlockItem,
    identifier_env: &mut Env<Entry>,
    labelenv: &mut Env<String>,
) -> Result<(), SemanticError> {
    match b {
        BlockItem::Statement(s) => resolve_statement(s, identifier_env, labelenv)?,
        BlockItem::Declaration(Declaration::Function(FunctionDeclaration(
            Identifier(name),
            _,
            Some(_),
        ))) => {
            return Err(SemanticError(format!(
                "invalid local defined function {}",
                name
            )))
        }
        BlockItem::Declaration(d) => resolve_declaration(d, identifier_env, labelenv)?,
    };

    Ok(())
}

fn resolve_declaration(
    d: &mut Declaration,
    identifier_env: &mut Env<Entry>,
    labelenv: &mut Env<String>,
) -> Result<(), SemanticError> {
    match d {
        Declaration::Variable(d) => resolve_variable_declaration(d, identifier_env)?,
        Declaration::Function(d) => resolve_function_declaration(d, identifier_env, labelenv)?,
    }

    Ok(())
}

fn resolve_variable_declaration(
    d: &mut VariableDeclaration,
    identifier_env: &mut Env<Entry>,
) -> Result<(), SemanticError> {
    let VariableDeclaration(Identifier(var_name), exp) = d;

    if identifier_env.contains_current(var_name) {
        return Err(SemanticError("Duplicate variable declaration".to_string()));
    }

    let name = make_temporary(&format!("var.{}", var_name));
    identifier_env.set(
        var_name.clone(),
        Entry {
            new_name: name.clone(),
            has_linkage: false,
        },
    );

    *var_name = name;

    if let Some(exp) = exp {
        resolve_exp(exp, identifier_env)?;
    };

    Ok(())
}

fn resolve_function_declaration(
    d: &mut FunctionDeclaration,
    identifier_env: &mut Env<Entry>,
    labelenv: &mut Env<String>,
) -> Result<(), SemanticError> {
    let FunctionDeclaration(Identifier(func_name), params, body) = d;

    if let Some(e) = identifier_env.get_current(func_name) {
        if e.has_linkage == false {
            return Err(SemanticError(format!(
                "Dupulicate Declaration {}",
                func_name
            )));
        }
    }
    identifier_env.set(
        func_name.to_string(),
        Entry {
            new_name: func_name.to_string(),
            has_linkage: true,
        },
    );

    let mut new_env = identifier_env.extend();
    for Identifier(param_name) in params {
        if new_env.contains_current(param_name) {
            return Err(SemanticError(format!("Dupulicate Param {}", param_name)));
        }

        let new_param_name = make_temporary(&format!("param.{}.{}", func_name, param_name));
        new_env.set(
            param_name.clone(),
            Entry {
                new_name: new_param_name.clone(),
                has_linkage: false,
            },
        );

        *param_name = new_param_name;
    }

    if let Some(body) = body {
        resolve_block(body, &mut new_env, labelenv)?;
    };

    Ok(())
}

fn resolve_statement(
    s: &mut Statement,
    identifier_env: &mut Env<Entry>,
    labelenv: &mut Env<String>,
) -> Result<(), SemanticError> {
    match s {
        Statement::Return(e) => resolve_exp(e, identifier_env)?,
        Statement::Expression(e) => resolve_exp(e, identifier_env)?,
        Statement::If(cond, then, el) => {
            resolve_exp(cond, identifier_env)?;
            resolve_statement(then, &mut identifier_env.extend(), labelenv)?;
            if let Some(el) = el {
                resolve_statement(el, &mut identifier_env.extend(), labelenv)?;
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

                resolve_statement(ls, identifier_env, labelenv)?;
            }
        }
        Statement::Compound(b) => resolve_identifier(b, &mut identifier_env.extend(), labelenv)?,
        Statement::While(cond, body, _id) => {
            resolve_exp(cond, identifier_env)?;
            resolve_statement(body, identifier_env, labelenv)?;
        }
        Statement::DoWhile(body, cond, _id) => {
            resolve_statement(body, identifier_env, labelenv)?;
            resolve_exp(cond, identifier_env)?;
        }
        Statement::For(init, cond, post, body, _id) => {
            let mut identifier_env = identifier_env.extend();

            match init {
                super::ForInit::Declaration(d) => {
                    resolve_declaration(d, &mut identifier_env, labelenv)?
                }
                super::ForInit::Expression(Some(e)) => resolve_exp(e, &identifier_env)?,
                _ => {}
            };

            if let Some(cond) = cond {
                resolve_exp(cond, &identifier_env)?;
            };

            if let Some(post) = post {
                resolve_exp(post, &identifier_env)?;
            };

            resolve_statement(body, &mut identifier_env, labelenv)?;
        }
        Statement::Switch(ctrl, body, _cases, _id) => {
            resolve_exp(ctrl, identifier_env)?;
            resolve_statement(body, identifier_env, labelenv)?;
        }
        Statement::Case(exp, body, _id) => {
            resolve_exp(exp, identifier_env)?;
            if let Some(body) = body {
                resolve_statement(body, identifier_env, labelenv)?;
            }
        }
        Statement::Default(body, _id) => {
            if let Some(body) = body {
                resolve_statement(body, identifier_env, labelenv)?;
            }
        }
        _ => {}
    }

    Ok(())
}

fn resolve_exp(exp: &mut Expression, identifier_env: &Env<Entry>) -> Result<(), SemanticError> {
    match exp {
        Expression::Var(Identifier(s)) => {
            if let Some(v) = identifier_env.get(&s) {
                *s = v.new_name.to_string();
            } else {
                return Err(SemanticError("Undeclared variable".to_string()));
            }
        }
        Expression::Assignment(_op, e1, e2) => {
            match e1.as_ref() {
                Expression::Var(Identifier(name)) => {
                    resolve_exp(e1, identifier_env)?;
                    resolve_exp(e2, identifier_env)?;
                }
                _ => return Err(SemanticError("Invalid lvalue".to_string())),
            };
        }
        Expression::Unary(op, e)
            if *op == UnaryOperator::IncrementPrefix
                || *op == UnaryOperator::IncrementPostfix
                || *op == UnaryOperator::DecrementPrefix
                || *op == UnaryOperator::DecrementPostfix =>
        {
            // 識別子のみに適用できる
            match e.as_ref() {
                Expression::Var(_) => resolve_exp(e, identifier_env)?,
                Expression::Unary(UnaryOperator::Complement, _) => resolve_exp(e, identifier_env)?,
                Expression::Unary(UnaryOperator::Not, _) => resolve_exp(e, identifier_env)?,
                Expression::Unary(UnaryOperator::Negate, _) => resolve_exp(e, identifier_env)?,
                _ => return Err(SemanticError("Invalid lvalue".to_string())),
            }
        }
        Expression::Unary(_op, e) => resolve_exp(e, identifier_env)?,
        Expression::Binary(_op, e1, e2) => {
            resolve_exp(e1, identifier_env)?;
            resolve_exp(e2, identifier_env)?;
        }
        Expression::Conditional(cond, e1, e2) => {
            resolve_exp(cond, identifier_env)?;
            resolve_exp(e1, identifier_env)?;
            resolve_exp(e2, identifier_env)?;
        }
        Expression::FunctionCall(Identifier(name), args) => {
            if let Some(e) = identifier_env.get(name) {
                *name = e.new_name.clone();
                for arg in args {
                    resolve_exp(arg, identifier_env)?;
                }
            } else {
                return Err(SemanticError(format!("Undeclared function {}", name)));
            };
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
