use rc::asm::{
    convert, BinaryOperator, Function, Identifier, Instruction, JumpCondition, Operand, Program,
    Register, UnaryOperator,
};
use rc::parse::{parse, validate};
use rc::tacky::convert as tconvert;
use rc::token;

#[test]
fn immediate() {
    let result = tokenize_to_convert(" int main(void) { return 1; } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(0),
                Instruction::Mov {
                    src: Operand::Immediate(1),
                    dst: Operand::Reg(Register::AX)
                },
                Instruction::Ret
            ]
        }])
    );
}

#[test]
fn allocate_stack() {
    let result = tokenize_to_convert(" int main(void) { return -(~(1)); } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(16),
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
        }])
    );
}

#[test]
fn binop1() {
    let result = tokenize_to_convert(" int main(void) { return 1+2; } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(16),
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
        }])
    );
}

#[test]
fn binop2() {
    let result = tokenize_to_convert(" int main(void) { return 1*2; } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(16),
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
        }])
    );
}

#[test]
fn binop3() {
    let result = tokenize_to_convert(" int main(void) { return 1/2; } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(16),
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
        }])
    );
}

#[test]
fn binop4() {
    let result = tokenize_to_convert(" int main(void) { return 2 * (3 + 4); } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(16),
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
        }])
    );
}

#[test]
fn binop5() {
    let result = tokenize_to_convert(" int main(void) { return 1 | 2 ^ 3 & 4 << 5 >> 6; } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(32),
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
        }]),
        "{result:#?}"
    );
}

#[test]
fn logical_operator() {
    let result = tokenize_to_convert(" int main(void) { return !1 && 2 || 3; } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(16),
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
        }]),
        "{result:#?}"
    );
}

#[test]
fn relational_operator() {
    // (1 == 2) != ((((3 < 4) > 5) <= 6) >= 7)
    let result = tokenize_to_convert(" int main(void) { return 1 == 2 != 3 < 4 > 5 <= 6 >= 7; } ");

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("main".to_string()),
            instructions: vec![
                Instruction::AllocateStack(32),
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
        }]),
        "{result:#?}"
    );
}

#[test]
fn function_definition_six_param() {
    let result = tokenize_to_convert(
        "
        int foo(int a, int b, int c, int d, int e, int f) {
            return a + b;
        }
        ",
    );

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("foo".to_string()),
            instructions: vec![
                Instruction::AllocateStack(32),
                Instruction::Mov {
                    src: Operand::Reg(Register::DI),
                    dst: Operand::Stack(-4),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::SI),
                    dst: Operand::Stack(-8),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::DX),
                    dst: Operand::Stack(-12),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::CX),
                    dst: Operand::Stack(-16),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R8),
                    dst: Operand::Stack(-20),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R9),
                    dst: Operand::Stack(-24),
                },
                Instruction::Mov {
                    src: Operand::Stack(-4),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R10),
                    dst: Operand::Stack(-28),
                },
                Instruction::Mov {
                    src: Operand::Stack(-8),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Binary(
                    BinaryOperator::Add,
                    Operand::Reg(Register::R10),
                    Operand::Stack(-28),
                ),
                Instruction::Mov {
                    src: Operand::Stack(-28),
                    dst: Operand::Reg(Register::AX),
                },
                Instruction::Ret
            ]
        },])
    );
}

#[test]
fn function_definition_seven_param() {
    let result = tokenize_to_convert(
        "
        int foo(int a, int b, int c, int d, int e, int f, int g) {
            return a + b;
        }
        ",
    );

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("foo".to_string()),
            instructions: vec![
                Instruction::AllocateStack(32),
                Instruction::Mov {
                    src: Operand::Reg(Register::DI),
                    dst: Operand::Stack(-4),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::SI),
                    dst: Operand::Stack(-8),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::DX),
                    dst: Operand::Stack(-12),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::CX),
                    dst: Operand::Stack(-16),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R8),
                    dst: Operand::Stack(-20),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R9),
                    dst: Operand::Stack(-24),
                },
                Instruction::Mov {
                    src: Operand::Stack(16),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R10),
                    dst: Operand::Stack(-28),
                },
                Instruction::Mov {
                    src: Operand::Stack(-4),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R10),
                    dst: Operand::Stack(-32),
                },
                Instruction::Mov {
                    src: Operand::Stack(-8),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Binary(
                    BinaryOperator::Add,
                    Operand::Reg(Register::R10),
                    Operand::Stack(-32),
                ),
                Instruction::Mov {
                    src: Operand::Stack(-32),
                    dst: Operand::Reg(Register::AX),
                },
                Instruction::Ret
            ]
        },])
    );
}

#[test]
fn function_definition_eight_param() {
    let result = tokenize_to_convert(
        "
        int foo(int a, int b, int c, int d, int e, int f, int g, int h) {
            return a + b;
        }
        ",
    );

    assert_eq!(
        result,
        Program::Program(vec![Function::Function {
            identifier: Identifier("foo".to_string()),
            instructions: vec![
                Instruction::AllocateStack(48),
                Instruction::Mov {
                    src: Operand::Reg(Register::DI),
                    dst: Operand::Stack(-4),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::SI),
                    dst: Operand::Stack(-8),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::DX),
                    dst: Operand::Stack(-12),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::CX),
                    dst: Operand::Stack(-16),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R8),
                    dst: Operand::Stack(-20),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R9),
                    dst: Operand::Stack(-24),
                },
                Instruction::Mov {
                    src: Operand::Stack(16),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R10),
                    dst: Operand::Stack(-28),
                },
                Instruction::Mov {
                    src: Operand::Stack(24),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R10),
                    dst: Operand::Stack(-32),
                },
                Instruction::Mov {
                    src: Operand::Stack(-4),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Mov {
                    src: Operand::Reg(Register::R10),
                    dst: Operand::Stack(-36),
                },
                Instruction::Mov {
                    src: Operand::Stack(-8),
                    dst: Operand::Reg(Register::R10),
                },
                Instruction::Binary(
                    BinaryOperator::Add,
                    Operand::Reg(Register::R10),
                    Operand::Stack(-36),
                ),
                Instruction::Mov {
                    src: Operand::Stack(-36),
                    dst: Operand::Reg(Register::AX),
                },
                Instruction::Ret
            ]
        },])
    );
}

#[test]
fn function_call_six_param() {
    let result = tokenize_to_convert(
        "
        int foo(int a, int b, int c, int d, int e, int f) {
            return a + b;
        }

        int main(void) {
            int a = 1;
            return foo(a, 2, 3, 4, 5, 6);
        }
        ",
    );

    assert_eq!(
        result,
        Program::Program(vec![
            Function::Function {
                identifier: Identifier("foo".to_string()),
                instructions: vec![
                    Instruction::AllocateStack(32),
                    Instruction::Mov {
                        src: Operand::Reg(Register::DI),
                        dst: Operand::Stack(-4),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::SI),
                        dst: Operand::Stack(-8),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::DX),
                        dst: Operand::Stack(-12),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::CX),
                        dst: Operand::Stack(-16),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R8),
                        dst: Operand::Stack(-20),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R9),
                        dst: Operand::Stack(-24),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(-4),
                        dst: Operand::Reg(Register::R10),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R10),
                        dst: Operand::Stack(-28),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(-8),
                        dst: Operand::Reg(Register::R10),
                    },
                    Instruction::Binary(
                        BinaryOperator::Add,
                        Operand::Reg(Register::R10),
                        Operand::Stack(-28),
                    ),
                    Instruction::Mov {
                        src: Operand::Stack(-28),
                        dst: Operand::Reg(Register::AX),
                    },
                    Instruction::Ret
                ]
            },
            Function::Function {
                identifier: Identifier("main".to_string()),
                instructions: vec![
                    Instruction::AllocateStack(16),
                    Instruction::Mov {
                        src: Operand::Immediate(1),
                        dst: Operand::Stack(-4),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(-4),
                        dst: Operand::Reg(Register::DI),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(2),
                        dst: Operand::Reg(Register::SI),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(3),
                        dst: Operand::Reg(Register::DX),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(4),
                        dst: Operand::Reg(Register::CX),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(5),
                        dst: Operand::Reg(Register::R8),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(6),
                        dst: Operand::Reg(Register::R9),
                    },
                    Instruction::Call(Identifier("foo".to_string())),
                    Instruction::Mov {
                        src: Operand::Reg(Register::AX),
                        dst: Operand::Stack(-8),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(-8),
                        dst: Operand::Reg(Register::AX),
                    },
                    Instruction::Ret
                ]
            }
        ])
    );
}

#[test]
fn function_call_eight_param() {
    let result = tokenize_to_convert(
        "
        int foo(int a, int b, int c, int d, int e, int f, int g, int h) {
            return a + b;
        }

        int main(void) {
            int a = 1;
            return foo(a, 2, 3, 4, 5, 6, 7, 8);
        }
        ",
    );

    assert_eq!(
        result,
        Program::Program(vec![
            Function::Function {
                identifier: Identifier("foo".to_string()),
                instructions: vec![
                    Instruction::AllocateStack(48),
                    Instruction::Mov {
                        src: Operand::Reg(Register::DI),
                        dst: Operand::Stack(-4),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::SI),
                        dst: Operand::Stack(-8),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::DX),
                        dst: Operand::Stack(-12),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::CX),
                        dst: Operand::Stack(-16),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R8),
                        dst: Operand::Stack(-20),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R9),
                        dst: Operand::Stack(-24),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(16),
                        dst: Operand::Reg(Register::R10),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R10),
                        dst: Operand::Stack(-28),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(24),
                        dst: Operand::Reg(Register::R10),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R10),
                        dst: Operand::Stack(-32),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(-4),
                        dst: Operand::Reg(Register::R10),
                    },
                    Instruction::Mov {
                        src: Operand::Reg(Register::R10),
                        dst: Operand::Stack(-36),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(-8),
                        dst: Operand::Reg(Register::R10),
                    },
                    Instruction::Binary(
                        BinaryOperator::Add,
                        Operand::Reg(Register::R10),
                        Operand::Stack(-36),
                    ),
                    Instruction::Mov {
                        src: Operand::Stack(-36),
                        dst: Operand::Reg(Register::AX),
                    },
                    Instruction::Ret
                ]
            },
            Function::Function {
                identifier: Identifier("main".to_string()),
                instructions: vec![
                    Instruction::AllocateStack(16),
                    Instruction::Mov {
                        src: Operand::Immediate(1),
                        dst: Operand::Stack(-4),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(-4),
                        dst: Operand::Reg(Register::DI),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(2),
                        dst: Operand::Reg(Register::SI),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(3),
                        dst: Operand::Reg(Register::DX),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(4),
                        dst: Operand::Reg(Register::CX),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(5),
                        dst: Operand::Reg(Register::R8),
                    },
                    Instruction::Mov {
                        src: Operand::Immediate(6),
                        dst: Operand::Reg(Register::R9),
                    },
                    Instruction::Push(Operand::Immediate(8)),
                    Instruction::Push(Operand::Immediate(7)),
                    Instruction::Call(Identifier("foo".to_string())),
                    Instruction::DeallocateStack(16),
                    Instruction::Mov {
                        src: Operand::Reg(Register::AX),
                        dst: Operand::Stack(-8),
                    },
                    Instruction::Mov {
                        src: Operand::Stack(-8),
                        dst: Operand::Reg(Register::AX),
                    },
                    Instruction::Ret
                ]
            }
        ])
    );
}

fn tokenize_to_convert(p: &str) -> Program {
    let mut result = token::tokenize(p.into()).unwrap();

    let mut result = parse(&mut result).unwrap();
    validate(&mut result).unwrap();

    let result = tconvert(result).unwrap();

    convert(result).unwrap()
}
