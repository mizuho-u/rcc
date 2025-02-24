use rc::parse::{parse, validate};
use rc::tacky::{self, convert, BinaryOperator, Identifier, Instruction, UnaryOperator, Val};
use rc::token;

#[test]
fn valid_tacky() {
    let result = tokenize_to_convert(" int main(void) { return 1; } ");
    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![tacky::Instruction::Return(tacky::Val::Constant(1))]
        ))
    )
}

#[test]
fn valid_tacky_unary() {
    let result = tokenize_to_convert(" int main(void) { return ~(-1); } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                tacky::Instruction::Unary(
                    tacky::UnaryOperator::Negate,
                    tacky::Val::Constant(1),
                    tacky::Val::Var(Identifier("tmp.1".to_string())),
                ),
                tacky::Instruction::Unary(
                    tacky::UnaryOperator::Complement,
                    tacky::Val::Var(Identifier("tmp.1".to_string())),
                    tacky::Val::Var(Identifier("tmp.2".to_string())),
                ),
                tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.2".to_string()))),
            ]
        ))
    )
}

#[test]
fn valid_tacky_binary() {
    let result = tokenize_to_convert(" int main(void) { return (1+2)*3-4/5; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::Add,
                    tacky::Val::Constant(1),
                    tacky::Val::Constant(2),
                    tacky::Val::Var(Identifier("tmp.1".to_string())),
                ),
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::Multiply,
                    tacky::Val::Var(Identifier("tmp.1".to_string())),
                    tacky::Val::Constant(3),
                    tacky::Val::Var(Identifier("tmp.2".to_string())),
                ),
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::Devide,
                    tacky::Val::Constant(4),
                    tacky::Val::Constant(5),
                    tacky::Val::Var(Identifier("tmp.3".to_string())),
                ),
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::Subtract,
                    tacky::Val::Var(Identifier("tmp.2".to_string())),
                    tacky::Val::Var(Identifier("tmp.3".to_string())),
                    tacky::Val::Var(Identifier("tmp.4".to_string())),
                ),
                tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.4".to_string()))),
            ]
        ))
    )
}

#[test]
fn bitwise_binary_operator() {
    let result = tokenize_to_convert(" int main(void) { return 1 | 2 ^ 3 & 4 << 5 >> 6; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::LeftShift,
                    tacky::Val::Constant(4),
                    tacky::Val::Constant(5),
                    tacky::Val::Var(Identifier("tmp.1".to_string())),
                ),
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::RightShift,
                    tacky::Val::Var(Identifier("tmp.1".to_string())),
                    tacky::Val::Constant(6),
                    tacky::Val::Var(Identifier("tmp.2".to_string())),
                ),
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::And,
                    tacky::Val::Constant(3),
                    tacky::Val::Var(Identifier("tmp.2".to_string())),
                    tacky::Val::Var(Identifier("tmp.3".to_string())),
                ),
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::Xor,
                    tacky::Val::Constant(2),
                    tacky::Val::Var(Identifier("tmp.3".to_string())),
                    tacky::Val::Var(Identifier("tmp.4".to_string())),
                ),
                tacky::Instruction::Binary(
                    tacky::BinaryOperator::Or,
                    tacky::Val::Constant(1),
                    tacky::Val::Var(Identifier("tmp.4".to_string())),
                    tacky::Val::Var(Identifier("tmp.5".to_string())),
                ),
                tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.5".to_string()))),
            ]
        ))
    )
}

#[test]
fn logical_operator() {
    let result = tokenize_to_convert(" int main(void) { return !1 && 2 || 3; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Unary(
                    UnaryOperator::Not,
                    Val::Constant(1),
                    Val::Var(Identifier("tmp.3".to_string())),
                ),
                Instruction::JumpIfZero(
                    Val::Var(Identifier("tmp.3".to_string())),
                    Identifier("false.3".to_string())
                ),
                Instruction::JumpIfZero(Val::Constant(2), Identifier("false.3".to_string())),
                Instruction::Copy(Val::Constant(1), Val::Var(Identifier("tmp.2".to_string()))),
                Instruction::Jump(Identifier("end.4".to_string())),
                Instruction::Label(Identifier("false.3".to_string())),
                Instruction::Copy(Val::Constant(0), Val::Var(Identifier("tmp.2".to_string()))),
                Instruction::Label(Identifier("end.4".to_string())),
                Instruction::JumpIfNotZero(
                    Val::Var(Identifier("tmp.2".to_string())),
                    Identifier("true.1".to_string())
                ),
                Instruction::JumpIfNotZero(Val::Constant(3), Identifier("true.1".to_string())),
                Instruction::Copy(Val::Constant(0), Val::Var(Identifier("tmp.1".to_string()))),
                Instruction::Jump(Identifier("end.2".to_string())),
                Instruction::Label(Identifier("true.1".to_string())),
                Instruction::Copy(Val::Constant(1), Val::Var(Identifier("tmp.1".to_string()))),
                Instruction::Label(Identifier("end.2".to_string())),
                tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.1".to_string()))),
            ]
        ))
    )
}

#[test]
fn relational_operator() {
    let result = tokenize_to_convert(" int main(void) { return 1 == 2 != 3 < 4 > 5 <= 6 >= 7; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Binary(
                    BinaryOperator::Equal,
                    Val::Constant(1),
                    Val::Constant(2),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::LessThan,
                    Val::Constant(3),
                    Val::Constant(4),
                    Val::Var(Identifier("tmp.2".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::GreaterThan,
                    Val::Var(Identifier("tmp.2".to_string())),
                    Val::Constant(5),
                    Val::Var(Identifier("tmp.3".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::LessOrEqual,
                    Val::Var(Identifier("tmp.3".to_string())),
                    Val::Constant(6),
                    Val::Var(Identifier("tmp.4".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::GreaterOrEqual,
                    Val::Var(Identifier("tmp.4".to_string())),
                    Val::Constant(7),
                    Val::Var(Identifier("tmp.5".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::NotEqual,
                    Val::Var(Identifier("tmp.1".to_string())),
                    Val::Var(Identifier("tmp.5".to_string())),
                    Val::Var(Identifier("tmp.6".to_string())),
                ),
                tacky::Instruction::Return(tacky::Val::Var(Identifier("tmp.6".to_string()))),
            ]
        ))
    )
}

#[test]
fn local_variables() {
    let result =
        tokenize_to_convert(" int main(void) { int b; int a = 10 + 1; b = a * 2; return b; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Constant(10),
                    Val::Constant(1),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.1".to_string())),
                    Val::Var(Identifier("var.a.2".to_string()))
                ),
                Instruction::Binary(
                    BinaryOperator::Multiply,
                    Val::Var(Identifier("var.a.2".to_string())),
                    Val::Constant(2),
                    Val::Var(Identifier("tmp.2".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.2".to_string())),
                    Val::Var(Identifier("var.b.1".to_string()))
                ),
                tacky::Instruction::Return(tacky::Val::Var(Identifier("var.b.1".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn function_with_no_return_statement() {
    let result = tokenize_to_convert(" int main(void) { int b; int a = 10 + 1; b = a * 2; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Constant(10),
                    Val::Constant(1),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.1".to_string())),
                    Val::Var(Identifier("var.a.2".to_string()))
                ),
                Instruction::Binary(
                    BinaryOperator::Multiply,
                    Val::Var(Identifier("var.a.2".to_string())),
                    Val::Constant(2),
                    Val::Var(Identifier("tmp.2".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.2".to_string())),
                    Val::Var(Identifier("var.b.1".to_string()))
                ),
                tacky::Instruction::Return(tacky::Val::Constant(0)),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn increment_decrement() {
    let result =
        tokenize_to_convert(" int main(void) { int a = 1; int b = ++a; a = b--; return a * b; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Copy(
                    Val::Constant(1),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(1),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.1".to_string())),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.1".to_string())),
                    Val::Var(Identifier("var.b.2".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Var(Identifier("tmp.2".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::Subtract,
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Constant(1),
                    Val::Var(Identifier("var.b.2".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.2".to_string())),
                    Val::Var(Identifier("var.a.1".to_string()))
                ),
                Instruction::Binary(
                    BinaryOperator::Multiply,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Var(Identifier("tmp.3".to_string())),
                ),
                tacky::Instruction::Return(Val::Var(Identifier("tmp.3".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

fn tokenize_to_convert(p: &str) -> tacky::Program {
    let mut result = token::tokenize(p.into()).unwrap();
    let result = parse(&mut result).unwrap();
    let result = validate(result).unwrap();

    convert(result).unwrap()
}
