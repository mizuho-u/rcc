use super::validate::{make_temporary, SemanticError};
use super::{Block, BlockItem, Identifier, Statement};

pub fn resolve_loop_label(body: &mut Block) -> Result<(), SemanticError> {
    match body {
        Block::Block(block_items) => {
            for b in block_items {
                if let BlockItem::Statement(s) = b {
                    label_statement(s, None)?;
                }
            }
        }
    };

    Ok(())
}

fn label_statement(s: &mut Statement, label: Option<&String>) -> Result<(), SemanticError> {
    match s {
        Statement::Break(Identifier(id)) => {
            if let Some(s) = label {
                *id = s.clone();
            } else {
                return Err(SemanticError("break statement outside of loop".to_string()));
            }
        }
        Statement::Continue(Identifier(id)) => {
            if let Some(s) = label {
                *id = s.clone();
            } else {
                return Err(SemanticError(
                    "continue statement outside of loop".to_string(),
                ));
            }
        }
        Statement::While(_e, s, Identifier(id)) => {
            let label = make_label(&"while".into());

            label_statement(s, Some(&label))?;
            *id = label;
        }
        Statement::DoWhile(s, _e, Identifier(id)) => {
            let label = make_label(&"dowhile".into());

            label_statement(s, Some(&label))?;
            *id = label;
        }
        Statement::For(_init, _cond, _post, s, Identifier(id)) => {
            let label = make_label(&"for".into());

            label_statement(s, Some(&label))?;
            *id = label;
        }
        Statement::If(_e, s1, s2) => {
            label_statement(s1, label)?;
            if let Some(o) = s2 {
                label_statement(o, label)?;
            }
        }
        Statement::Label(_id, s) => label_statement(s, label)?,
        Statement::Compound(b) => match b {
            Block::Block(items) => {
                for item in items {
                    if let BlockItem::Statement(s) = item {
                        label_statement(s, label)?;
                    }
                }
            }
        },
        _ => {}
    };

    Ok(())
}

fn make_label(p: &String) -> String {
    make_temporary(p)
}
