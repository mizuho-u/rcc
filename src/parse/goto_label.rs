use super::validate::SemanticError;
use super::Expression;
use super::{env::Env, Block, BlockItem, Identifier, Statement};

pub fn resolve_goto_label(body: &mut Block, labelmap: &Env<String>) -> Result<(), SemanticError> {
    resolve_block(body, labelmap)
}

fn resolve_statement(s: &mut Statement, labelmap: &Env<String>) -> Result<(), SemanticError> {
    match s {
        Statement::Label(_id, ls) => {
            resolve_statement(ls, labelmap)?;

            Ok(())
        }
        Statement::Goto(id) => {
            if let Some(label) = labelmap.get(&id.0) {
                *s = Statement::Goto(Identifier(label.clone()));
                Ok(())
            } else {
                Err(SemanticError(format!("undefied label {}", &id.0)))
            }
        }
        Statement::If(_, then, el) => {
            resolve_statement(then, labelmap)?;

            if let Some(el) = el {
                resolve_statement(el, labelmap)?;
            }

            Ok(())
        }
        Statement::Compound(b) => resolve_block(b, labelmap),
        Statement::While(_cond, body, _id) => resolve_statement(body, labelmap),
        Statement::DoWhile(body, _cond, _id) => resolve_statement(body, labelmap),
        Statement::For(_init, _cond, _post, body, _id) => resolve_statement(body, labelmap),
        Statement::Switch(_exp, body, _cases, _id) => resolve_statement(body, labelmap),
        Statement::Case(exp, s, _id) => {
            if !matches!(exp, Expression::Constant(_)) {
                return Err(SemanticError(format!(
                    "case statement values must be constant"
                )));
            }

            if let Some(s) = s {
                resolve_statement(s, labelmap)?;
            }

            Ok(())
        }
        Statement::Default(s, _id) => {
            if let Some(s) = s {
                resolve_statement(s, labelmap)?;
            }

            Ok(())
        }
        _ => Ok(()),
    }
}

fn resolve_block(b: &mut Block, labelmap: &Env<String>) -> Result<(), SemanticError> {
    let Block::Block(body) = b;

    for b in body {
        if let BlockItem::Statement(s) = b {
            resolve_statement(s, labelmap)?;
        }
    }

    Ok(())
}
