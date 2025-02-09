use crate::tacky;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Function),
}

#[derive(PartialEq, Debug)]
pub enum Function {
    Function {
        identifier: Identifier,
        instructions: Vec<Instruction>,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary(UnaryOperator, Operand),
    AllocateStack(i32),
    Ret,
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Operand {
    Immediate(i32),
    Reg(Register),
    Pseudo(Identifier),
    Stack(i32),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Register {
    AX,
    R10,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier {
    pub s: String,
}

#[derive(Debug)]
pub struct AssemblyError {
    s: String,
}

pub fn convert(p: tacky::Program) -> Result<Program, AssemblyError> {
    let mut p = convert_program(p)?;
    replace_pseudo(&mut p)?;
    fix_instructions(&mut p)?;
    Ok(p)
}

fn convert_program(p: tacky::Program) -> Result<Program, AssemblyError> {
    let tacky::Program::Program(func) = p;

    Ok(Program::Program(convert_function(func)?))
}

fn convert_function(f: tacky::Function) -> Result<Function, AssemblyError> {
    let tacky::Function::Function(id, body) = f;

    Ok(Function::Function {
        identifier: Identifier { s: id.s },
        instructions: convert_statement(body)?,
    })
}

fn convert_statement(s: Vec<tacky::Instruction>) -> Result<Vec<Instruction>, AssemblyError> {
    let mut insts = Vec::new();

    for inst in s {
        match inst {
            tacky::Instruction::Return(val) => {
                insts.push(Instruction::Mov {
                    src: convert_exp(val)?,
                    dst: Operand::Reg(Register::AX),
                });
                insts.push(Instruction::Ret);
            }
            tacky::Instruction::Unary(op, src, dst) => {
                let op: UnaryOperator = match op {
                    tacky::UnaryOperator::Complement => UnaryOperator::Not,
                    tacky::UnaryOperator::Negate => UnaryOperator::Neg,
                };

                let src = convert_exp(src)?;
                let dst = convert_exp(dst)?;

                insts.push(Instruction::Mov {
                    src: src,
                    dst: dst.clone(),
                });
                insts.push(Instruction::Unary(op, dst));
            }
            tacky::Instruction::Binary(op, left, right, dst) => todo!(),
        }
    }

    Ok(insts)
}

fn convert_exp(e: tacky::Val) -> Result<Operand, AssemblyError> {
    let op = match e {
        tacky::Val::Constant(n) => Operand::Immediate(n),
        tacky::Val::Var(id) => Operand::Pseudo(Identifier { s: id.s }),
    };

    Ok(op)
}

fn replace_pseudo(p: &mut Program) -> Result<(), AssemblyError> {
    let Program::Program(f) = p;
    let Function::Function {
        identifier: _,
        instructions,
    } = f;

    for inst in instructions {
        match inst {
            Instruction::Mov { src, dst } => {
                replace_operand(src)?;
                replace_operand(dst)?;
            }
            Instruction::Unary(_, o) => replace_operand(o)?,
            Instruction::AllocateStack(_) => {}
            Instruction::Ret => {}
        }
    }

    Ok(())
}

fn replace_operand(o: &mut Operand) -> Result<(), AssemblyError> {
    match o {
        Operand::Pseudo(id) => {
            *o = Operand::Stack(allocate_stack(&id.s));
        }
        _ => {}
    }

    Ok(())
}

fn allocate_stack(id: &String) -> i32 {
    thread_local!(
        pub static COUNT: Rc<RefCell<i32>> = Rc::new(RefCell::new(0));

        pub static MAP_MUT: RefCell<HashMap<String, i32>> = {
            let m = HashMap::new();
            RefCell::new(m)
        }
    );

    let mut stack = 0;

    MAP_MUT.with(|m| {
        let mut m_ref = m.borrow_mut();

        if let Some(cnt) = m_ref.get(id) {
            stack = *cnt;
        } else {
            let rc: Rc<RefCell<i32>> = get_stack();
            let mut m = rc.borrow_mut();
            *m -= 4;
            stack = *m;

            m_ref.insert(id.to_string(), stack);
        }
    });

    stack
}

fn get_stack() -> Rc<RefCell<i32>> {
    thread_local!(
        pub static COUNT: Rc<RefCell<i32>> = Rc::new(RefCell::new(0));
    );

    COUNT.with(|rc| rc.clone())
}

fn fix_instructions(p: &mut Program) -> Result<(), AssemblyError> {
    let Program::Program(f) = p;
    let Function::Function {
        identifier: _,
        ref mut instructions,
    } = f;

    insert_allocate_stack(instructions)?;
    *instructions = rewrite_stack_operand(instructions)?;

    Ok(())
}

fn insert_allocate_stack(instructions: &mut Vec<Instruction>) -> Result<(), AssemblyError> {
    instructions.insert(0, Instruction::AllocateStack(-*get_stack().borrow()));

    Ok(())
}

fn rewrite_stack_operand(
    instructions: &Vec<Instruction>,
) -> Result<Vec<Instruction>, AssemblyError> {
    let mut rewrited = Vec::new();

    for inst in instructions {
        match inst {
            Instruction::Mov { src, dst } => match (src, dst) {
                (Operand::Stack(_), Operand::Stack(_)) => {
                    rewrited.push(Instruction::Mov {
                        src: src.clone(),
                        dst: Operand::Reg(Register::R10),
                    });
                    rewrited.push(Instruction::Mov {
                        src: Operand::Reg(Register::R10),
                        dst: dst.clone(),
                    });
                }
                _ => rewrited.push(inst.clone()),
            },
            _ => rewrited.push(inst.clone()),
        }
    }

    Ok(rewrited)
}

#[cfg(test)]
mod tests {
    use crate::asm::{
        Function, Identifier, Instruction, Operand, Program, Register, UnaryOperator,
    };
    use crate::parse::parse;
    use crate::tacky::convert as tconvert;
    use crate::token;

    use super::convert;

    #[test]
    fn valid_asm() {
        let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = tconvert(result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            Program::Program(Function::Function {
                identifier: Identifier {
                    s: "main".to_string()
                },
                instructions: vec![
                    Instruction::AllocateStack(0),
                    Instruction::Mov {
                        src: Operand::Immediate(1),
                        dst: Operand::Reg(Register::AX)
                    },
                    Instruction::Ret
                ]
            })
        );
    }

    #[test]
    fn allocate_stack() {
        let mut result = token::tokenize(" int main(void) { return -(~(1)); } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        let result = tconvert(result).unwrap();
        let result = convert(result).unwrap();

        assert_eq!(
            result,
            Program::Program(Function::Function {
                identifier: Identifier {
                    s: "main".to_string()
                },
                instructions: vec![
                    Instruction::AllocateStack(8),
                    Instruction::Mov {
                        src: Operand::Immediate(1),
                        dst: Operand::Stack(-4)
                    },
                    Instruction::Unary(UnaryOperator::Not, Operand::Stack(-4)),
                    Instruction::Mov {
                        src: Operand::Stack(-4),
                        dst: Operand::Reg(Register::R10)
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R10),
                        dst: Operand::Stack(-8)
                    },
                    Instruction::Unary(UnaryOperator::Neg, Operand::Stack(-8)),
                    Instruction::Mov {
                        src: Operand::Stack(-8),
                        dst: Operand::Reg(Register::AX)
                    },
                    Instruction::Ret
                ]
            })
        );
    }
}
