use crate::tacky;
use std::collections::HashMap;

#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Vec<Function>),
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
    DeallocateStack(i32),
    Push(Operand),
    Call(Identifier),
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
    DI,
    SI,
    R8,
    R9,
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

struct Stack {
    current: i32,
    ids: HashMap<String, i32>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            current: 0,
            ids: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, k: &String) -> i32 {
        if let Some(v) = self.ids.get(k) {
            *v
        } else {
            self.current -= 4;
            self.ids.insert(k.to_string(), self.current);
            self.current
        }
    }
}

type StackMap = HashMap<String, Stack>;

pub fn convert(p: tacky::Program) -> Result<Program, AssemblyError> {
    let mut p = convert_program(p)?;
    let mut stack: StackMap = StackMap::new();
    replace_pseudo(&mut p, &mut stack)?;
    fix_instructions(&mut p, &stack)?;
    Ok(p)
}

fn convert_program(p: tacky::Program) -> Result<Program, AssemblyError> {
    let tacky::Program::Program(fs) = p;

    let fs = fs
        .iter()
        .map(|f| convert_function(f))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Program::Program(fs))
}

fn convert_function(f: &tacky::Function) -> Result<Function, AssemblyError> {
    let tacky::Function::Function(id, params, body) = f;

    let mut instructions = Vec::new();
    for (i, p) in params.iter().enumerate() {
        // 6個まではレジスタ
        if let Some(r) = get_param_register(i) {
            instructions.push(Instruction::Mov {
                src: Operand::Reg(r),
                dst: Operand::Pseudo(Identifier(p.0.to_string())),
            });
        } else {
            // 7個以上はスタックに積まれる
            let offset: i32 = (16 + (i - 6) * 8).try_into().map_err(|_| {
                AssemblyError(format!("copy params at index {} to the stack failed.", i))
            })?;

            instructions.push(Instruction::Mov {
                src: Operand::Stack(offset),
                dst: Operand::Pseudo(Identifier(p.0.to_string())),
            });
        }
    }

    instructions.append(&mut convert_statement(body)?);

    Ok(Function::Function {
        identifier: Identifier(id.0.to_string()),
        instructions: instructions,
    })
}

fn convert_statement(s: &Vec<tacky::Instruction>) -> Result<Vec<Instruction>, AssemblyError> {
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
                insts.push(Instruction::Jmp(Identifier(identifier.0.to_string())));
            }
            tacky::Instruction::JumpIfZero(v, identifier) => {
                insts.push(Instruction::Cmp(Operand::Immediate(0), convert_exp(v)?));
                insts.push(Instruction::JmpCC(
                    JumpCondition::E,
                    Identifier(identifier.0.to_string()),
                ));
            }
            tacky::Instruction::JumpIfNotZero(v, identifier) => {
                insts.push(Instruction::Cmp(Operand::Immediate(0), convert_exp(v)?));
                insts.push(Instruction::JmpCC(
                    JumpCondition::NE,
                    Identifier(identifier.0.to_string()),
                ));
            }
            tacky::Instruction::Label(identifier) => {
                insts.push(Instruction::Label(Identifier(identifier.0.to_string())));
            }
            tacky::Instruction::FunctionCall(name, args, dst) => {
                insts.append(&mut convert_function_call(name, args, dst)?);
            }
        }
    }

    Ok(insts)
}

fn convert_function_call(
    name: &tacky::Identifier,
    args: &Vec<tacky::Val>,
    dst: &tacky::Val,
) -> Result<Vec<Instruction>, AssemblyError> {
    let mut insts = Vec::new();

    let register_args;
    let mut stack_args = vec![];

    if args.len() <= 6 {
        register_args = args.clone();
    } else {
        let (l, r) = args.split_at(6);
        register_args = l.to_vec();
        stack_args = r.to_vec();
    }

    // 16byteアライメント
    let stack_padding = if stack_args.len() % 2 == 0 { 0 } else { 8 };
    if stack_padding != 0 {
        insts.push(Instruction::AllocateStack(stack_padding));
    }

    // レジスタ経由の引数
    for (i, arg) in register_args.iter().enumerate() {
        if let Some(r) = get_param_register(i) {
            let arg = convert_exp(arg)?;
            insts.push(Instruction::Mov {
                src: arg,
                dst: Operand::Reg(r),
            });
        }
    }

    // スタック経由の引数
    for arg in stack_args.iter().rev() {
        let arg = convert_exp(arg)?;
        match arg {
            Operand::Immediate(_) | Operand::Reg(_) => {
                insts.push(Instruction::Push(arg));
            }
            // 4byteはレジスタにコピーして8byteでpush
            _ => {
                insts.push(Instruction::Mov {
                    src: arg,
                    dst: Operand::Reg(Register::AX),
                });
                insts.push(Instruction::Push(Operand::Reg(Register::AX)));
            }
        }
    }

    // 関数呼び出し
    insts.push(Instruction::Call(Identifier(name.0.to_string())));

    // スタック経由の引数を除外
    let bytes_to_remove = 8 * stack_args.len() as i32 + stack_padding;
    if bytes_to_remove != 0 {
        insts.push(Instruction::DeallocateStack(bytes_to_remove));
    }

    // 関数の結果はAXに入っているのでコピー先に入れる
    insts.push(Instruction::Mov {
        src: Operand::Reg(Register::AX),
        dst: convert_exp(dst)?,
    });

    Ok(insts)
}

fn convert_binop(op: &tacky::BinaryOperator) -> Result<BinaryOperator, AssemblyError> {
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

fn convert_uop(op: &tacky::UnaryOperator) -> Result<UnaryOperator, AssemblyError> {
    match op {
        tacky::UnaryOperator::Complement => Ok(UnaryOperator::Not),
        tacky::UnaryOperator::Negate => Ok(UnaryOperator::Neg),
        _ => Err(AssemblyError(format!("cannot convert unary op"))),
    }
}

fn jump_condition_from(op: &tacky::BinaryOperator) -> Result<JumpCondition, AssemblyError> {
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

fn convert_exp(e: &tacky::Val) -> Result<Operand, AssemblyError> {
    let op = match e {
        tacky::Val::Constant(n) => Operand::Immediate(*n),
        tacky::Val::Var(id) => Operand::Pseudo(Identifier(id.0.to_string())),
    };

    Ok(op)
}

fn replace_pseudo(p: &mut Program, stack_map: &mut StackMap) -> Result<(), AssemblyError> {
    let Program::Program(fs) = p;

    for f in fs {
        let Function::Function {
            identifier: f_name,
            instructions,
        } = f;

        stack_map.insert(f_name.0.to_string(), Stack::new());
        let mut stack = stack_map.get_mut(&f_name.0).unwrap();

        for inst in instructions {
            match inst {
                Instruction::Mov { src, dst } => {
                    replace_operand(&mut stack, src)?;
                    replace_operand(&mut stack, dst)?;
                }
                Instruction::Unary(_, o) => replace_operand(&mut stack, o)?,
                Instruction::Binary(_, src, dst) => {
                    replace_operand(&mut stack, src)?;
                    replace_operand(&mut stack, dst)?;
                }
                Instruction::Idiv(operand) => {
                    replace_operand(&mut stack, operand)?;
                }
                Instruction::Cmp(o1, o2) => {
                    replace_operand(&mut stack, o1)?;
                    replace_operand(&mut stack, o2)?;
                }
                Instruction::SetCC(_, o) => replace_operand(&mut stack, o)?,
                Instruction::Push(o) => replace_operand(&mut stack, o)?,
                _ => {}
            }
        }
    }

    Ok(())
}

fn replace_operand(stack: &mut Stack, op: &mut Operand) -> Result<(), AssemblyError> {
    match op {
        Operand::Pseudo(id) => {
            *op = Operand::Stack(stack.allocate(&id.0));
        }
        _ => {}
    }

    Ok(())
}

fn get_param_register(index: usize) -> Option<Register> {
    thread_local!(
        static PARAM_REGISTER: Vec<Register> = vec![
            Register::DI,
            Register::SI,
            Register::DX,
            Register::CX,
            Register::R8,
            Register::R9,
        ];
    );

    PARAM_REGISTER.with(|rs| rs.get(index).and_then(|v| Some(v.clone())))
}

fn fix_instructions(p: &mut Program, stack_map: &StackMap) -> Result<(), AssemblyError> {
    let Program::Program(fs) = p;

    for f in fs {
        let Function::Function {
            identifier: f_name,
            ref mut instructions,
        } = f;

        let stack = stack_map.get(&f_name.0).unwrap();

        insert_allocate_stack(stack, instructions)?;
        *instructions = rewrite_stack_operand(instructions)?;
    }

    Ok(())
}

fn insert_allocate_stack(
    stack: &Stack,
    instructions: &mut Vec<Instruction>,
) -> Result<(), AssemblyError> {
    let stack_size = if stack.current % 16 == 0 {
        -stack.current
    } else {
        -stack.current + (16 + stack.current % 16)
    };

    instructions.insert(0, Instruction::AllocateStack(stack_size));

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
