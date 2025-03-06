use super::validate::{make_temporary, SemanticError};
use super::{parser, Block, BlockItem, Identifier, Statement};

enum Label {
    Loop(String),
    Switch(String),
}

pub fn resolve_loop_label(body: &mut Block) -> Result<(), SemanticError> {
    match body {
        Block::Block(block_items) => {
            for b in block_items {
                if let BlockItem::Statement(s) = b {
                    label_statement(s, &mut vec![], &mut vec![])?;
                }
            }
        }
    };

    Ok(())
}

fn label_statement(
    s: &mut Statement,
    labels: &mut Vec<Label>,
    cases: &mut Vec<(Option<parser::Expression>, parser::Identifier)>,
) -> Result<(), SemanticError> {
    match s {
        Statement::Break(Identifier(id)) => {
            if labels.len() == 0 {
                return Err(SemanticError(
                    "break statement outside of loop or switch".to_string(),
                ));
            }

            if let Some(l) = labels.last() {
                match l {
                    Label::Loop(s) => {
                        *id = s.to_string();
                    }
                    Label::Switch(s) => {
                        *id = s.to_string();
                    }
                }
            }
        }
        Statement::Continue(Identifier(id)) => {
            let closest_loop = labels.iter().rev().find(|x| matches!(x, Label::Loop(_)));
            if let Some(Label::Loop(s)) = closest_loop {
                *id = s.to_string();
            } else {
                return Err(SemanticError(
                    "continue statement outside of loop".to_string(),
                ));
            }
        }
        Statement::While(_e, s, Identifier(id)) => {
            let label = make_label(&"while".into());
            labels.push(Label::Loop(label.clone()));

            label_statement(s, labels, cases)?;

            labels.pop();

            *id = label;
        }
        Statement::DoWhile(s, _e, Identifier(id)) => {
            let label = make_label(&"dowhile".into());
            labels.push(Label::Loop(label.clone()));

            label_statement(s, labels, cases)?;

            labels.pop();

            *id = label;
        }
        Statement::For(_init, _cond, _post, s, Identifier(id)) => {
            let label = make_label(&"for".into());
            labels.push(Label::Loop(label.clone()));

            label_statement(s, labels, cases)?;

            labels.pop();

            *id = label;
        }
        Statement::If(_e, s1, s2) => {
            label_statement(s1, labels, cases)?;

            if let Some(o) = s2 {
                label_statement(o, labels, cases)?;
            }
        }
        Statement::Label(_id, s) => label_statement(s, labels, cases)?,
        Statement::Compound(b) => match b {
            Block::Block(items) => {
                for item in items {
                    if let BlockItem::Statement(s) = item {
                        label_statement(s, labels, cases)?;
                    }
                }
            }
        },
        Statement::Switch(_ctrl, s, cases, Identifier(id)) => {
            let label = make_label(&"switch".into());
            labels.push(Label::Switch(label.clone()));

            label_statement(s, labels, cases)?;

            labels.pop();
            *id = label;
        }
        Statement::Case(exp, body, Identifier(id)) => {
            let closest_switch = labels.iter().rev().find(|x| matches!(x, Label::Switch(_)));
            if let Some(Label::Switch(s)) = closest_switch {
                *id = make_temporary(s);
            } else {
                return Err(SemanticError("case statement outside of loop".to_string()));
            }

            if let Some(body) = body {
                label_statement(body, labels, cases)?;
            }

            for l in &mut *cases {
                if let (Some(le), _) = l {
                    if le == exp {
                        return Err(SemanticError("dulicate cases".to_string()));
                    }
                };
            }

            cases.push((Some(exp.clone()), Identifier(id.clone())));
        }
        Statement::Default(body, Identifier(id)) => {
            let closest_switch = labels.iter().rev().find(|x| matches!(x, Label::Switch(_)));
            if let Some(Label::Switch(s)) = closest_switch {
                *id = s.to_string();
            } else {
                return Err(SemanticError(
                    "default statement outside of loop".to_string(),
                ));
            }

            if let Some(body) = body {
                label_statement(body, labels, cases)?;
            }

            for l in &mut *cases {
                if let (None, _) = l {
                    return Err(SemanticError("dulicate defaults".to_string()));
                };
            }

            cases.push((None, Identifier(id.clone())));
        }
        _ => {}
    };

    Ok(())
}

fn make_label(p: &String) -> String {
    make_temporary(p)
}
