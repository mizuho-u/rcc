use std::cell::RefCell;
use std::collections::HashMap;

use super::{
    label::validate_label, Block, BlockItem, Declaration, Expression, Function, Identifier,
    Program, Statement, UnaryOperator,
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
    let mut var: HashMap<String, String> = HashMap::new();
    let mut label: HashMap<String, String> = HashMap::new();

    let Program::Program(Function::Function(id, Block::Block(body))) = p;

    let mut r_body = Vec::new();
    for b in body {
        r_body.push(resolve_block_item(b, &mut var, &mut label)?);
    }

    validate_label(&mut r_body, &label)?;

    Ok(Program::Program(Function::Function(
        id,
        Block::Block(r_body),
    )))
}

fn resolve_block_item(
    b: BlockItem,
    varmap: &mut HashMap<String, String>,
    labelmap: &mut HashMap<String, String>,
) -> Result<BlockItem, SemanticError> {
    match b {
        BlockItem::Statement(s) => Ok(BlockItem::Statement(resolve_statement(
            s, varmap, labelmap,
        )?)),
        BlockItem::Declaration(declaration) => match declaration {
            Declaration::Declaration(Identifier(s), exp) => {
                if varmap.contains_key(&s) {
                    return Err(SemanticError("Duplicate variable declaration".to_string()));
                }

                let name = make_temporary(&format!("var.{}", &s));
                varmap.insert(s.clone(), name.clone());

                let exp = if let Some(exp) = exp {
                    Some(resolve_exp(exp, varmap)?)
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
    varmap: &mut HashMap<String, String>,
    labelmap: &mut HashMap<String, String>,
) -> Result<Statement, SemanticError> {
    match s {
        Statement::Return(e) => Ok(Statement::Return(resolve_exp(e, varmap)?)),
        Statement::Expression(e) => Ok(Statement::Expression(resolve_exp(e, varmap)?)),
        Statement::Null => Ok(Statement::Null),
        Statement::If(cond, then, el) => {
            let cond = resolve_exp(cond, varmap)?;
            let then = resolve_statement(*then, varmap, labelmap)?;
            if let Some(el) = el {
                let el = resolve_statement(*el, varmap, labelmap)?;
                Ok(Statement::If(cond, Box::new(then), Some(Box::new(el))))
            } else {
                Ok(Statement::If(cond, Box::new(then), None))
            }
        }
        Statement::Goto(id) => Ok(Statement::Goto(id)),
        Statement::Label(id, ls) => {
            if let Some(s) = labelmap.get(&id.0) {
                Ok(Statement::Label(Identifier(s.clone()), ls))
            } else {
                let name = make_temporary(&format!("lbl.{}", &id.0));
                labelmap.insert(id.0, name.clone());

                Ok(Statement::Label(
                    Identifier(name),
                    Box::new(resolve_statement(*ls, varmap, labelmap)?),
                ))
            }
        }
        Statement::Compound(block) => todo!(),
    }
}

fn resolve_exp(
    exp: Expression,
    varmap: &HashMap<String, String>,
) -> Result<Expression, SemanticError> {
    match exp {
        Expression::Var(Identifier(s)) => {
            if let Some(v) = varmap.get(&s) {
                Ok(Expression::Var(Identifier(v.clone())))
            } else {
                Err(SemanticError("Undeclared variable".to_string()))
            }
        }
        Expression::Assignment(op, e1, e2) => match *e1 {
            Expression::Var(_) => Ok(Expression::Assignment(
                op,
                Box::new(resolve_exp(*e1, varmap)?),
                Box::new(resolve_exp(*e2, varmap)?),
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
                Expression::Var(_) => Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varmap)?))),
                Expression::Unary(UnaryOperator::Complement, _) => {
                    Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varmap)?)))
                }
                Expression::Unary(UnaryOperator::Not, _) => {
                    Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varmap)?)))
                }
                Expression::Unary(UnaryOperator::Negate, _) => {
                    Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varmap)?)))
                }
                _ => Err(SemanticError("Invalid lvalue".to_string())),
            }
        }
        Expression::Unary(op, e) => Ok(Expression::Unary(op, Box::new(resolve_exp(*e, varmap)?))),
        Expression::Binary(op, e1, e2) => Ok(Expression::Binary(
            op,
            Box::new(resolve_exp(*e1, varmap)?),
            Box::new(resolve_exp(*e2, varmap)?),
        )),
        Expression::Conditional(cond, e1, e2) => Ok(Expression::Conditional(
            Box::new(resolve_exp(*cond, varmap)?),
            Box::new(resolve_exp(*e1, varmap)?),
            Box::new(resolve_exp(*e2, varmap)?),
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
