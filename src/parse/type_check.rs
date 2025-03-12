use super::validate::SemanticError;
use super::{
    parser, Block, Declaration, Expression, ForInit, FunctionDeclaration, Identifier, Statement,
    VariableDeclaration,
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct TypeError(String);

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for TypeError {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub struct SymbolTable {
    table: HashMap<String, Entry>,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Fun(usize),
}

pub struct Entry {
    pub name: String,
    pub r#type: Type,
    pub defined: bool,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
        }
    }

    pub fn get(&self, k: &String) -> Option<&Entry> {
        self.table.get(k)
    }

    pub fn set(&mut self, k: String, e: Entry) {
        self.table.insert(k, e);
    }
}

pub fn type_check(p: &parser::Program) -> Result<SymbolTable, SemanticError> {
    let mut table = SymbolTable::new();

    let parser::Program::Program(fs) = p;

    for f in fs {
        check_function_declaration(f, &mut table)?;
    }

    return Ok(table);
}

fn check_function_declaration(
    f: &FunctionDeclaration,
    table: &mut SymbolTable,
) -> Result<(), SemanticError> {
    let FunctionDeclaration(Identifier(name), params, body) = f;

    let mut new_e = Entry {
        name: name.clone(),
        r#type: Type::Fun(params.len()),
        defined: body.is_some(),
    };

    let e = table.get(&new_e.name);
    if let Some(e) = e {
        if e.r#type != new_e.r#type {
            return Err(SemanticError(format!(
                "Incompatible function declarations {}",
                new_e.name
            )));
        }

        if e.defined && new_e.defined {
            return Err(SemanticError(format!(
                "Function is define more than once {}",
                new_e.name
            )));
        }

        new_e.defined |= e.defined;
    }

    table.set(name.to_string(), new_e);

    if let Some(body) = body {
        for Identifier(p) in params {
            table.set(
                p.clone(),
                Entry {
                    name: p.to_string(),
                    r#type: Type::Int,
                    defined: false,
                },
            );
        }

        check_block(body, table)?;
    }

    Ok(())
}

fn check_block(block: &Block, table: &mut SymbolTable) -> Result<(), SemanticError> {
    let Block::Block(block_items) = block;

    for b in block_items {
        match b {
            super::BlockItem::Statement(s) => check_statement(s, table)?,
            super::BlockItem::Declaration(Declaration::Function(f)) => {
                check_function_declaration(f, table)?
            }
            super::BlockItem::Declaration(Declaration::Variable(v)) => {
                check_variable_declaration(v, table)?
            }
        }
    }

    Ok(())
}

fn check_variable_declaration(
    v: &VariableDeclaration,
    table: &mut SymbolTable,
) -> Result<(), SemanticError> {
    let VariableDeclaration(Identifier(name), init) = v;
    table.set(
        name.to_string(),
        Entry {
            name: name.to_string(),
            r#type: Type::Int,
            defined: false,
        },
    );

    if let Some(init) = init {
        check_expression(init, table)?;
    }

    Ok(())
}

fn check_expression(e: &Expression, table: &mut SymbolTable) -> Result<(), SemanticError> {
    match e {
        Expression::Var(Identifier(name)) => {
            if let Some(e) = table.get(name) {
                if e.r#type != Type::Int {
                    return Err(SemanticError(format!(
                        "function name used as variable {}",
                        name
                    )));
                }
            }
        }
        Expression::Unary(_op, e) => check_expression(e, table)?,
        Expression::Binary(_op, e1, e2) => {
            check_expression(e1, table)?;
            check_expression(e2, table)?;
        }
        Expression::Assignment(_op, e1, e2) => {
            check_expression(e1, table)?;
            check_expression(e2, table)?;
        }
        Expression::Conditional(cond, e1, e2) => {
            check_expression(cond, table)?;
            check_expression(e1, table)?;
            check_expression(e2, table)?;
        }
        Expression::FunctionCall(Identifier(name), params) => {
            if let Some(e) = table.get(name) {
                match e.r#type {
                    Type::Int => {
                        return Err(SemanticError(format!(
                            "function name used as variable {}",
                            name
                        )));
                    }
                    Type::Fun(s) => {
                        if params.len() != s {
                            return Err(SemanticError(format!(
                                "wrong number of arguments. expected {}, got {}",
                                s,
                                params.len()
                            )));
                        }

                        for p in params {
                            check_expression(p, table)?;
                        }
                    }
                }
            }
        }
        _ => {}
    }

    Ok(())
}

fn check_statement(s: &Statement, table: &mut SymbolTable) -> Result<(), SemanticError> {
    match s {
        Statement::Return(e) => check_expression(e, table)?,
        Statement::Expression(e) => check_expression(e, table)?,
        Statement::If(e, then, r#else) => {
            check_expression(e, table)?;
            check_statement(then, table)?;
            if let Some(r#else) = r#else {
                check_statement(r#else, table)?;
            }
        }
        Statement::Label(_id, s) => {
            check_statement(s, table)?;
        }
        Statement::Compound(block) => {
            check_block(block, table)?;
        }
        Statement::While(cond, body, _name) => {
            check_expression(cond, table)?;
            check_statement(body, table)?;
        }
        Statement::DoWhile(body, cond, _name) => {
            check_statement(body, table)?;
            check_expression(cond, table)?;
        }
        Statement::For(init, cond, post, body, _name) => {
            match init {
                ForInit::Declaration(Declaration::Variable(v)) => {
                    check_variable_declaration(v, table)?;
                }
                ForInit::Declaration(Declaration::Function(_)) => {
                    return Err(SemanticError(format!("declare function in forinit.")))
                }
                ForInit::Expression(e) => {
                    if let Some(e) = e {
                        check_expression(e, table)?;
                    }
                }
            };

            if let Some(cond) = cond {
                check_expression(cond, table)?;
            }

            if let Some(post) = post {
                check_expression(post, table)?;
            }

            check_statement(body, table)?;
        }
        Statement::Switch(e, s, _cases, _name) => {
            check_expression(e, table)?;
            check_statement(s, table)?;
        }
        Statement::Case(e, s, _name) => {
            check_expression(e, table)?;
            if let Some(s) = s {
                check_statement(s, table)?;
            }
        }
        Statement::Default(s, _name) => {
            if let Some(s) = s {
                check_statement(s, table)?;
            }
        }
        _ => {}
    }

    Ok(())
}
