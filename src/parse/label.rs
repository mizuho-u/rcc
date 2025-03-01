use std::collections::HashMap;

use super::validate::SemanticError;
use super::{BlockItem, Identifier, Statement};

pub fn validate_label(
    body: &mut Vec<BlockItem>,
    labelmap: &HashMap<String, String>,
) -> Result<(), SemanticError> {
    let mut check: HashMap<String, bool> = HashMap::new();

    for b in body {
        if let BlockItem::Statement(s) = b {
            resolve_statement_item(s, labelmap, &mut check)?;
        }
    }

    Ok(())
}

fn resolve_statement_item(
    s: &mut Statement,
    labelmap: &HashMap<String, String>,
    check: &mut HashMap<String, bool>,
) -> Result<(), SemanticError> {
    match s {
        Statement::Label(id, ls) => {
            if let Some(_) = check.get(&id.0) {
                return Err(SemanticError(format!("duplicated label {}", &id.0)));
            } else {
                check.insert(id.0.clone(), true);
            }

            resolve_statement_item(ls, labelmap, check)?;

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
            resolve_statement_item(then, labelmap, check)?;

            if let Some(el) = el {
                resolve_statement_item(el, labelmap, check)?;
            }

            Ok(())
        }
        _ => Ok(()),
    }
}
