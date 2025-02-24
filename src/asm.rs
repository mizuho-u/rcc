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
    Binary(BinaryOperator, Operand, Operand),
    Cmp(Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jmp(Identifier),
    JmpCC(JumpCondition, Identifier),
    SetCC(JumpCondition, Operand),
    Label(Identifier),
    AllocateStack(i32),
    Ret,
}

#[derive(PartialEq, Debug, Clone)]
pub enum JumpCondition {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    And,
    Or,
    Xor,
    Shl,
    Shr,
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
    CX,
    DX,
    R10,
    R11,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct AssemblyError(String);

impl std::fmt::Display for AssemblyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for AssemblyError {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl std::error::Error for AssemblyError {}

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
        identifier: Identifier(id.0),
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
                let src = convert_exp(src)?;
                let dst = convert_exp(dst)?;

                match op {
                    tacky::UnaryOperator::Complement | tacky::UnaryOperator::Negate => {
                        insts.push(Instruction::Mov {
                            src: src,
                            dst: dst.clone(),
                        });
                        insts.push(Instruction::Unary(convert_uop(op)?, dst));
                    }
                    tacky::UnaryOperator::Not => {
                        insts.push(Instruction::Cmp(Operand::Immediate(0), src));
                        insts.push(Instruction::Mov {
                            src: Operand::Immediate(0),
                            dst: dst.clone(),
                        });
                        insts.push(Instruction::SetCC(JumpCondition::E, dst));
                    }
                };
            }
            tacky::Instruction::Binary(op, left, right, dst) => {
                let left = convert_exp(left)?;
                let right = convert_exp(right)?;
                let dst = convert_exp(dst)?;

                match op {
                    tacky::BinaryOperator::Add
                    | tacky::BinaryOperator::Subtract
                    | tacky::BinaryOperator::Multiply
                    | tacky::BinaryOperator::And
                    | tacky::BinaryOperator::Or
                    | tacky::BinaryOperator::Xor
                    | tacky::BinaryOperator::LeftShift
                    | tacky::BinaryOperator::RightShift => {
                        insts.push(Instruction::Mov {
                            src: left,
                            dst: dst.clone(),
                        });
                        insts.push(Instruction::Binary(convert_binop(op)?, right, dst));
                    }
                    tacky::BinaryOperator::Divide => {
                        insts.push(Instruction::Mov {
                            src: left,
                            dst: Operand::Reg(Register::AX),
                        });
                        insts.push(Instruction::Cdq);
                        insts.push(Instruction::Idiv(right));
                        // idivの商はEAXにセットされる
                        insts.push(Instruction::Mov {
                            src: Operand::Reg(Register::AX),
                            dst: dst,
                        });
                    }
                    tacky::BinaryOperator::Remainder => {
                        insts.push(Instruction::Mov {
                            src: left,
                            dst: Operand::Reg(Register::AX),
                        });
                        insts.push(Instruction::Cdq);
                        insts.push(Instruction::Idiv(right));
                        // idivの余りはEDXにセットされる
                        insts.push(Instruction::Mov {
                            src: Operand::Reg(Register::DX),
                            dst: dst,
                        });
                    }
                    tacky::BinaryOperator::Equal
                    | tacky::BinaryOperator::NotEqual
                    | tacky::BinaryOperator::LessThan
                    | tacky::BinaryOperator::LessOrEqual
                    | tacky::BinaryOperator::GreaterThan
                    | tacky::BinaryOperator::GreaterOrEqual => {
                        // AT&T記法でオペランドが逆になる
                        // 1 greater than 2
                        // cmp 2, 1
                        // 1 - 2
                        // ZF = 0, SF = 1
                        insts.push(Instruction::Cmp(right, left));
                        insts.push(Instruction::Mov {
                            src: Operand::Immediate(0),
                            dst: dst.clone(),
                        });
                        insts.push(Instruction::SetCC(jump_condition_from(op)?, dst));
                    }
                };
            }
            tacky::Instruction::Copy(v1, v2) => {
                insts.push(Instruction::Mov {
                    src: convert_exp(v1)?,
                    dst: convert_exp(v2)?,
                });
            }
            tacky::Instruction::Jump(identifier) => {
                insts.push(Instruction::Jmp(Identifier(identifier.0)));
            }
            tacky::Instruction::JumpIfZero(v, identifier) => {
                insts.push(Instruction::Cmp(Operand::Immediate(0), convert_exp(v)?));
                insts.push(Instruction::JmpCC(
                    JumpCondition::E,
                    Identifier(identifier.0),
                ));
            }
            tacky::Instruction::JumpIfNotZero(v, identifier) => {
                insts.push(Instruction::Cmp(Operand::Immediate(0), convert_exp(v)?));
                insts.push(Instruction::JmpCC(
                    JumpCondition::NE,
                    Identifier(identifier.0),
                ));
            }
            tacky::Instruction::Label(identifier) => {
                insts.push(Instruction::Label(Identifier(identifier.0)));
            }
        }
    }

    Ok(insts)
}

fn convert_binop(op: tacky::BinaryOperator) -> Result<BinaryOperator, AssemblyError> {
    match op {
        tacky::BinaryOperator::Add => Ok(BinaryOperator::Add),
        tacky::BinaryOperator::Subtract => Ok(BinaryOperator::Sub),
        tacky::BinaryOperator::Multiply => Ok(BinaryOperator::Mult),
        tacky::BinaryOperator::And => Ok(BinaryOperator::And),
        tacky::BinaryOperator::Or => Ok(BinaryOperator::Or),
        tacky::BinaryOperator::Xor => Ok(BinaryOperator::Xor),
        tacky::BinaryOperator::LeftShift => Ok(BinaryOperator::Shl),
        tacky::BinaryOperator::RightShift => Ok(BinaryOperator::Shr),
        _ => Err(AssemblyError(format!("cannot convert binary op"))),
    }
}

fn convert_uop(op: tacky::UnaryOperator) -> Result<UnaryOperator, AssemblyError> {
    match op {
        tacky::UnaryOperator::Complement => Ok(UnaryOperator::Not),
        tacky::UnaryOperator::Negate => Ok(UnaryOperator::Neg),
        _ => Err(AssemblyError(format!("cannot convert unary op"))),
    }
}

fn jump_condition_from(op: tacky::BinaryOperator) -> Result<JumpCondition, AssemblyError> {
    match op {
        tacky::BinaryOperator::Equal => Ok(JumpCondition::E),
        tacky::BinaryOperator::NotEqual => Ok(JumpCondition::NE),
        tacky::BinaryOperator::LessThan => Ok(JumpCondition::L),
        tacky::BinaryOperator::LessOrEqual => Ok(JumpCondition::LE),
        tacky::BinaryOperator::GreaterThan => Ok(JumpCondition::G),
        tacky::BinaryOperator::GreaterOrEqual => Ok(JumpCondition::GE),
        _ => Err(AssemblyError(format!(
            "cannot convert binary op to jump condition"
        ))),
    }
}

fn convert_exp(e: tacky::Val) -> Result<Operand, AssemblyError> {
    let op = match e {
        tacky::Val::Constant(n) => Operand::Immediate(n),
        tacky::Val::Var(id) => Operand::Pseudo(Identifier(id.0)),
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
            Instruction::Binary(_, src, dst) => {
                replace_operand(src)?;
                replace_operand(dst)?;
            }
            Instruction::Idiv(operand) => {
                replace_operand(operand)?;
            }
            Instruction::Cmp(o1, o2) => {
                replace_operand(o1)?;
                replace_operand(o2)?;
            }
            Instruction::SetCC(_, o) => replace_operand(o)?,
            _ => {}
        }
    }

    Ok(())
}

fn replace_operand(o: &mut Operand) -> Result<(), AssemblyError> {
    match o {
        Operand::Pseudo(id) => {
            *o = Operand::Stack(allocate_stack(&id.0));
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
                // スタック -> スタックのコピーはできないのでレジスタを経由
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
            //　divのオペランドに即値はとれない
            Instruction::Idiv(o @ Operand::Immediate(_)) => {
                rewrited.push(Instruction::Mov {
                    src: o.clone(),
                    dst: Operand::Reg(Register::R10),
                });
                rewrited.push(Instruction::Idiv(Operand::Reg(Register::R10)));
            }
            //　addとsubのオペランドの両方にスタックをとることはできない
            Instruction::Binary(op, Operand::Stack(s1), Operand::Stack(s2))
                if (*op == BinaryOperator::Add)
                    || (*op == BinaryOperator::Sub)
                    || (*op == BinaryOperator::And)
                    || (*op == BinaryOperator::Or)
                    || (*op == BinaryOperator::Xor) =>
            {
                rewrited.push(Instruction::Mov {
                    src: Operand::Stack(*s1),
                    dst: Operand::Reg(Register::R10),
                });
                rewrited.push(Instruction::Binary(
                    op.clone(),
                    Operand::Reg(Register::R10),
                    Operand::Stack(*s2),
                ));
            }
            //　shiftのカウンタは即値かCLレジスタ
            Instruction::Binary(op, Operand::Stack(s), dst)
                if (*op == BinaryOperator::Shl) || (*op == BinaryOperator::Shr) =>
            {
                rewrited.push(Instruction::Mov {
                    src: Operand::Stack(*s),
                    dst: Operand::Reg(Register::CX),
                });
                // 5bitマスクするらしい
                rewrited.push(Instruction::Binary(
                    BinaryOperator::And,
                    Operand::Immediate(0x1f),
                    Operand::Reg(Register::CX),
                ));
                rewrited.push(Instruction::Binary(
                    op.clone(),
                    Operand::Reg(Register::CX),
                    dst.clone(),
                ));
            }
            // mulのオペランドのdestinationにメモリは指定できない
            Instruction::Binary(BinaryOperator::Mult, left, Operand::Stack(dst)) => {
                rewrited.push(Instruction::Mov {
                    src: Operand::Stack(*dst),
                    dst: Operand::Reg(Register::R11),
                });
                rewrited.push(Instruction::Binary(
                    BinaryOperator::Mult,
                    left.clone(),
                    Operand::Reg(Register::R11),
                ));
                rewrited.push(Instruction::Mov {
                    src: Operand::Reg(Register::R11),
                    dst: Operand::Stack(*dst),
                });
            }
            // cmpのオペランドの両方にスタックをとることはできない
            Instruction::Cmp(Operand::Stack(s1), Operand::Stack(s2)) => {
                rewrited.push(Instruction::Mov {
                    src: Operand::Stack(*s1),
                    dst: Operand::Reg(Register::R10),
                });
                rewrited.push(Instruction::Cmp(
                    Operand::Reg(Register::R10),
                    Operand::Stack(*s2),
                ));
            }
            // cmpのリザルトに即値は指定できない
            Instruction::Cmp(o, imm @ Operand::Immediate(_)) => {
                rewrited.push(Instruction::Mov {
                    src: imm.clone(),
                    dst: Operand::Reg(Register::R11),
                });
                rewrited.push(Instruction::Cmp(o.clone(), Operand::Reg(Register::R11)));
            }
            _ => rewrited.push(inst.clone()),
        }
    }

    Ok(rewrited)
}
