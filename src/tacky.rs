use crate::ast;
use std::cell::RefCell;

#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Function),
}

#[derive(PartialEq, Debug)]
pub enum Function {
    Function(Identifier, Vec<Instruction>),
}

#[derive(PartialEq, Debug)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOperator, Val, Val),
    Binary(BinaryOperator, Val, Val, Val),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Constant(i32),
    Var(Identifier),
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Devide,
    Remainder,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier {
    pub s: String,
}

#[derive(Debug)]
pub struct TackeyError {
    s: String,
}

pub fn convert(p: ast::Program) -> Result<Program, TackeyError> {
    convert_program(p)
}

fn convert_program(p: ast::Program) -> Result<Program, TackeyError> {
    let ast::Program::Program(f) = p;

    Ok(Program::Program(convert_function(f)?))
}

fn convert_function(f: ast::Function) -> Result<Function, TackeyError> {
    let ast::Function::Function(id, stmt) = f;

    Ok(Function::Function(
        Identifier { s: id.s },
        convert_statement(stmt)?,
    ))
}

fn convert_statement(s: ast::Statement) -> Result<Vec<Instruction>, TackeyError> {
    let mut instructions = Vec::new();

    match s {
        ast::Statement::Return(e) => {
            let val = convert_exp(e, &mut instructions)?;
            instructions.push(Instruction::Return(val));
        }
    }

    Ok(instructions)
}

fn convert_exp(
    e: ast::Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Val, TackeyError> {
    match e {
        ast::Expression::Constant(n) => Ok(Val::Constant(n)),
        ast::Expression::Unary(op, e) => {
            let src = convert_exp(*e, instructions)?;
            let dst = Val::Var(Identifier {
                s: make_temporary(),
            });

            let op = match op {
                ast::UnaryOperator::Complement => UnaryOperator::Complement,
                ast::UnaryOperator::Negate => UnaryOperator::Negate,
            };

            instructions.push(Instruction::Unary(op, src, dst.clone()));

            Ok(dst)
        }
        ast::Expression::Binary(op, left, right) => {
            let left = convert_exp(*left, instructions)?;
            let right = convert_exp(*right, instructions)?;

            let dst = Val::Var(Identifier {
                s: make_temporary(),
            });

            let op = match op {
                ast::BinaryOperator::Subtract => BinaryOperator::Subtract,
                ast::BinaryOperator::Add => BinaryOperator::Add,
                ast::BinaryOperator::Multiply => BinaryOperator::Multiply,
                ast::BinaryOperator::Divide => BinaryOperator::Devide,
                ast::BinaryOperator::Remainder => BinaryOperator::Remainder,
            };

            instructions.push(Instruction::Binary(op, left, right, dst.clone()));

            Ok(dst)
        }
    }
}

fn make_temporary() -> String {
    thread_local!(
        pub static COUNT: RefCell<i32> = RefCell::new(0)
    );

    let mut count = 0;

    COUNT.with(|c| {
        let mut c = c.borrow_mut();
        count = *c;
        *c += 1;
    });

    format!("tmp.{}", count)
}

#[cfg(test)]
mod tests {
    use crate::parse::parse;
    use crate::tacky::{self, convert, Identifier};
    use crate::token;

    #[test]
    fn valid_tacky() {
        let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                vec![tacky::Instruction::Return(tacky::Val::Constant(1))]
            ))
        )
    }

    #[test]
    fn valid_tacky_unary() {
        let mut result = token::tokenize(" int main(void) { return ~(-1); } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                vec![
                    tacky::Instruction::Unary(
                        tacky::UnaryOperator::Negate,
                        tacky::Val::Constant(1),
                        tacky::Val::Var(Identifier {
                            s: "tmp.0".to_string()
                        }),
                    ),
                    tacky::Instruction::Unary(
                        tacky::UnaryOperator::Complement,
                        tacky::Val::Var(Identifier {
                            s: "tmp.0".to_string()
                        }),
                        tacky::Val::Var(Identifier {
                            s: "tmp.1".to_string()
                        }),
                    ),
                    tacky::Instruction::Return(tacky::Val::Var(Identifier {
                        s: "tmp.1".to_string()
                    })),
                ]
            ))
        )
    }

    #[test]
    fn valid_tacky_binary() {
        let mut result =
            token::tokenize(" int main(void) { return (1+2)*3-4/5; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            tacky::Program::Program(tacky::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                vec![
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Add,
                        tacky::Val::Constant(1),
                        tacky::Val::Constant(2),
                        tacky::Val::Var(Identifier {
                            s: "tmp.0".to_string()
                        }),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Multiply,
                        tacky::Val::Var(Identifier {
                            s: "tmp.0".to_string()
                        }),
                        tacky::Val::Constant(3),
                        tacky::Val::Var(Identifier {
                            s: "tmp.1".to_string()
                        }),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Devide,
                        tacky::Val::Constant(4),
                        tacky::Val::Constant(5),
                        tacky::Val::Var(Identifier {
                            s: "tmp.2".to_string()
                        }),
                    ),
                    tacky::Instruction::Binary(
                        tacky::BinaryOperator::Subtract,
                        tacky::Val::Var(Identifier {
                            s: "tmp.1".to_string()
                        }),
                        tacky::Val::Var(Identifier {
                            s: "tmp.2".to_string()
                        }),
                        tacky::Val::Var(Identifier {
                            s: "tmp.3".to_string()
                        }),
                    ),
                    tacky::Instruction::Return(tacky::Val::Var(Identifier {
                        s: "tmp.3".to_string()
                    })),
                ]
            ))
        )
    }
}
