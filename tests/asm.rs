use rc::asm::{
    convert, BinaryOperator, Function, Identifier, Instruction, JumpCondition, Operand, Program,
    Register, UnaryOperator,
};
use rc::parse::parse;
use rc::tacky::convert as tconvert;
use rc::token;

#[test]
fn immediate() {
    let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    let result = tconvert(result).unwrap();
    let result = convert(result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function {
            identifier: Identifier("main".to_string()),
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
            identifier: Identifier("main".to_string()),
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

#[test]
fn binop1() {
    let mut result = token::tokenize(" int main(void) { return 1+2; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    let result = tconvert(result).unwrap();
    let result = convert(result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(4),
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Stack(-4)
                },
                Instruction::Binary(
                    BinaryOperator::Add,
                    Operand::Immediate(2),
                    Operand::Stack(-4)
                ),
                Instruction::Mov {
                    src: Operand::Stack(-4),
                    dst: Operand::Reg(Register::AX)
                },
                Instruction::Ret
            ]
        })
    );
}

#[test]
fn binop2() {
    let mut result = token::tokenize(" int main(void) { return 1*2; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    let result = tconvert(result).unwrap();
    let result = convert(result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(4),
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Stack(-4)
                },
                Instruction::Mov {
                    src: Operand::Stack(-4),
                    dst: Operand::Reg(Register::R11),
                },
                Instruction::Binary(
                    BinaryOperator::Mult,
                    Operand::Immediate(2),
                    Operand::Reg(Register::R11)
                ),
                Instruction::Mov {
                    src: Operand::Reg(Register::R11),
                    dst: Operand::Stack(-4),
                },
                Instruction::Mov {
                    src: Operand::Stack(-4),
                    dst: Operand::Reg(Register::AX)
                },
                Instruction::Ret
            ]
        })
    );
}

#[test]
fn binop3() {
    let mut result = token::tokenize(" int main(void) { return 1/2; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    let result = tconvert(result).unwrap();
    let result = convert(result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(4),
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Reg(Register::AX),
                },
                Instruction::Cdq,
                Instruction::Mov {
                    src: Operand::Immediate(2),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Idiv(Operand::Reg(Register::R10)),
                Instruction::Mov {
                    src: Operand::Reg(Register::AX),
                    dst: Operand::Stack(-4),
                },
                Instruction::Mov {
                    src: Operand::Stack(-4),
                    dst: Operand::Reg(Register::AX)
                },
                Instruction::Ret
            ]
        })
    );
}

#[test]
fn binop4() {
    let mut result = token::tokenize(" int main(void) { return 2 * (3 + 4); } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    let result = tconvert(result).unwrap();
    let result = convert(result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(8),
                Instruction::Mov {
                    src: Operand::Immediate(3),
                    dst: Operand::Stack(-4)
                },
                Instruction::Binary(
                    BinaryOperator::Add,
                    Operand::Immediate(4),
                    Operand::Stack(-4)
                ),
                Instruction::Mov {
                    src: Operand::Immediate(2),
                    dst: Operand::Stack(-8)
                },
                Instruction::Mov {
                    src: Operand::Stack(-8),
                    dst: Operand::Reg(Register::R11)
                },
                Instruction::Binary(
                    BinaryOperator::Mult,
                    Operand::Stack(-4),
                    Operand::Reg(Register::R11),
                ),
                Instruction::Mov {
                    src: Operand::Reg(Register::R11),
                    dst: Operand::Stack(-8),
                },
                Instruction::Mov {
                    src: Operand::Stack(-8),
                    dst: Operand::Reg(Register::AX),
                },
                Instruction::Ret
            ]
        })
    );
}

#[test]
fn binop5() {
    let mut result =
        token::tokenize(" int main(void) { return 1 | 2 ^ 3 & 4 << 5 >> 6; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    let result = tconvert(result).unwrap();
    let result = convert(result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(20),
                Instruction::Mov {
                    src: Operand::Immediate(4),
                    dst: Operand::Stack(-4)
                },
                Instruction::Binary(
                    BinaryOperator::Shl,
                    Operand::Immediate(5),
                    Operand::Stack(-4)
                ),
                Instruction::Mov {
                    src: Operand::Stack(-4),
                    dst: Operand::Reg(Register::R10)
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R10),
                    dst: Operand::Stack(-8),
                },
                Instruction::Binary(
                    BinaryOperator::Shr,
                    Operand::Immediate(6),
                    Operand::Stack(-8),
                ),
                Instruction::Mov {
                    src: Operand::Immediate(3),
                    dst: Operand::Stack(-12),
                },
                Instruction::Mov {
                    src: Operand::Stack(-8),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Binary(
                    BinaryOperator::And,
                    Operand::Reg(Register::R10),
                    Operand::Stack(-12),
                ),
                Instruction::Mov {
                    src: Operand::Immediate(2),
                    dst: Operand::Stack(-16),
                },
                Instruction::Mov {
                    src: Operand::Stack(-12),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Binary(
                    BinaryOperator::Xor,
                    Operand::Reg(Register::R10),
                    Operand::Stack(-16),
                ),
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Stack(-20),
                },
                Instruction::Mov {
                    src: Operand::Stack(-16),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Binary(
                    BinaryOperator::Or,
                    Operand::Reg(Register::R10),
                    Operand::Stack(-20),
                ),
                Instruction::Mov {
                    src: Operand::Stack(-20),
                    dst: Operand::Reg(Register::AX),
                },
                Instruction::Ret
            ]
        }),
        "{result:#?}"
    );
}

#[test]
fn logical_operator() {
    let mut result = token::tokenize(" int main(void) { return !1 && 2 || 3; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    let result = tconvert(result).unwrap();
    let result = convert(result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(12),
                // !1
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Reg(Register::R11)
                },
                Instruction::Cmp(Operand::Immediate(0), Operand::Reg(Register::R11)),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-4),
                },
                Instruction::SetCC(JumpCondition::E, Operand::Stack(-4)),
                // (!1 &&)
                Instruction::Cmp(Operand::Immediate(0), Operand::Stack(-4)),
                Instruction::JmpCC(JumpCondition::E, Identifier("false.3".to_string())),
                // 2
                Instruction::Mov {
                    src: Operand::Immediate(2),
                    dst: Operand::Reg(Register::R11)
                },
                Instruction::Cmp(Operand::Immediate(0), Operand::Reg(Register::R11)),
                Instruction::JmpCC(JumpCondition::E, Identifier("false.3".to_string())),
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Stack(-8),
                },
                Instruction::Jmp(Identifier("end.4".to_string())),
                Instruction::Label(Identifier("false.3".to_string())),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-8),
                },
                Instruction::Label(Identifier("end.4".to_string())),
                // ((!1 && 2) ||)
                Instruction::Cmp(Operand::Immediate(0), Operand::Stack(-8)),
                Instruction::JmpCC(JumpCondition::NE, Identifier("true.1".to_string())),
                // 3
                Instruction::Mov {
                    src: Operand::Immediate(3),
                    dst: Operand::Reg(Register::R11)
                },
                Instruction::Cmp(Operand::Immediate(0), Operand::Reg(Register::R11)),
                Instruction::JmpCC(JumpCondition::NE, Identifier("true.1".to_string())),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-12),
                },
                Instruction::Jmp(Identifier("end.2".to_string())),
                Instruction::Label(Identifier("true.1".to_string())),
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Stack(-12),
                },
                Instruction::Label(Identifier("end.2".to_string())),
                Instruction::Mov {
                    src: Operand::Stack(-12),
                    dst: Operand::Reg(Register::AX),
                },
                Instruction::Ret
            ]
        }),
        "{result:#?}"
    );
}

#[test]
fn relational_operator() {
    // (1 == 2) != ((((3 < 4) > 5) <= 6) >= 7)
    let mut result =
        token::tokenize(" int main(void) { return 1 == 2 != 3 < 4 > 5 <= 6 >= 7; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();
    let result = tconvert(result).unwrap();
    let result = convert(result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(24),
                // (1 == 2)
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Reg(Register::R11),
                },
                Instruction::Cmp(Operand::Immediate(2), Operand::Reg(Register::R11)),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-4),
                },
                Instruction::SetCC(JumpCondition::E, Operand::Stack(-4)),
                // (3 < 4)
                Instruction::Mov {
                    src: Operand::Immediate(3),
                    dst: Operand::Reg(Register::R11),
                },
                Instruction::Cmp(Operand::Immediate(4), Operand::Reg(Register::R11)),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-8),
                },
                Instruction::SetCC(JumpCondition::L, Operand::Stack(-8)),
                // ((3 < 4) > 5)
                Instruction::Cmp(Operand::Immediate(5), Operand::Stack(-8)),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-12),
                },
                Instruction::SetCC(JumpCondition::G, Operand::Stack(-12)),
                // (((3 < 4) > 5) <= 6)
                Instruction::Cmp(Operand::Immediate(6), Operand::Stack(-12)),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-16),
                },
                Instruction::SetCC(JumpCondition::LE, Operand::Stack(-16)),
                // ((((3 < 4) > 5) <= 6) >= 7)
                Instruction::Cmp(Operand::Immediate(7), Operand::Stack(-16)),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-20),
                },
                Instruction::SetCC(JumpCondition::GE, Operand::Stack(-20)),
                // (1 == 2) != ((((3 < 4) > 5) <= 6) >= 7)
                Instruction::Mov {
                    src: Operand::Stack(-20),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Cmp(Operand::Reg(Register::R10), Operand::Stack(-4)),
                Instruction::Mov {
                    src: Operand::Immediate(0),
                    dst: Operand::Stack(-24),
                },
                Instruction::SetCC(JumpCondition::NE, Operand::Stack(-24)),
                Instruction::Mov {
                    src: Operand::Stack(-24),
                    dst: Operand::Reg(Register::AX),
                },
                Instruction::Ret
            ]
        }),
        "{result:#?}"
    );
}
