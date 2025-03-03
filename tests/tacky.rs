use rc::parse::{parse, validate};
use rc::tacky::{
    self, convert, BinaryOperator, Function, Identifier, Instruction, Program, UnaryOperator, Val,
};
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
                    tacky::BinaryOperator::Divide,
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

#[test]
fn compound_assignment() {
    let result = tokenize_to_convert(" int main(void) { int a = 0; a += 2; return a; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Copy(
                    Val::Constant(0),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(2),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.1".to_string())),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                tacky::Instruction::Return(Val::Var(Identifier("var.a.1".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn if_statement() {
    let result = tokenize_to_convert(" int main(void) { if(1) return 2; return 3; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::JumpIfZero(Val::Constant(1), Identifier("endif.1".to_string())),
                tacky::Instruction::Return(Val::Constant(2)),
                tacky::Instruction::Label(Identifier("endif.1".to_string())),
                tacky::Instruction::Return(Val::Constant(3)),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn if_else_statement() {
    let result = tokenize_to_convert(" int main(void) { if(1) return 2; else return 3; } ");

    assert_eq!(
        result,
        tacky::Program::Program(tacky::Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::JumpIfZero(Val::Constant(1), Identifier("else.2".to_string())),
                tacky::Instruction::Return(Val::Constant(2)),
                Instruction::Jump(Identifier("endif.1".to_string())),
                tacky::Instruction::Label(Identifier("else.2".to_string())),
                tacky::Instruction::Return(Val::Constant(3)),
                tacky::Instruction::Label(Identifier("endif.1".to_string())),
                // returnがない判定で挿入されてしまう・・そのうち治るはず
                tacky::Instruction::Return(Val::Constant(0)),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn conditional_expression() {
    let result = tokenize_to_convert(" int main(void) { return 1 ? 2 : 3; } ");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::JumpIfZero(Val::Constant(1), Identifier("e2.2".to_string())),
                Instruction::Copy(Val::Constant(2), Val::Var(Identifier("tmp.1".to_string()))),
                Instruction::Jump(Identifier("endcond.1".to_string())),
                Instruction::Label(Identifier("e2.2".to_string())),
                Instruction::Copy(Val::Constant(3), Val::Var(Identifier("tmp.1".to_string()))),
                Instruction::Label(Identifier("endcond.1".to_string())),
                tacky::Instruction::Return(Val::Var(Identifier("tmp.1".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn goto() {
    let result = tokenize_to_convert(
        " int main(void) { int a = 0; int b = 1; goto label; int c = 1; label: b = 2; return b; } ",
    );

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Copy(
                    Val::Constant(0),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Copy(
                    Val::Constant(1),
                    Val::Var(Identifier("var.b.2".to_string())),
                ),
                Instruction::Jump(Identifier("lbl.label.4".to_string())),
                Instruction::Copy(
                    Val::Constant(1),
                    Val::Var(Identifier("var.c.3".to_string())),
                ),
                Instruction::Label(Identifier("lbl.label.4".to_string())),
                Instruction::Copy(
                    Val::Constant(2),
                    Val::Var(Identifier("var.b.2".to_string()))
                ),
                tacky::Instruction::Return(Val::Var(Identifier("var.b.2".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn for_loop() {
    let result = tokenize_to_convert(
        " int main(void) { int a = 0; for( int b = 0; b < 10; b++ ) { if(a == 6) continue; a += 2; } return a; } ",
    );

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Copy(
                    Val::Constant(0),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Copy(
                    Val::Constant(0),
                    Val::Var(Identifier("var.b.2".to_string())),
                ),
                Instruction::Label(Identifier("start.for.3".to_string())),
                Instruction::Binary(
                    BinaryOperator::LessThan,
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Constant(10),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::JumpIfZero(
                    Val::Var(Identifier("tmp.1".to_string())),
                    Identifier("break.for.3".to_string())
                ),
                Instruction::Binary(
                    BinaryOperator::Equal,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(6),
                    Val::Var(Identifier("tmp.2".to_string())),
                ),
                Instruction::JumpIfZero(
                    Val::Var(Identifier("tmp.2".to_string())),
                    Identifier("endif.1".to_string())
                ),
                Instruction::Jump(Identifier("continue.for.3".to_string())),
                Instruction::Label(Identifier("endif.1".to_string())),
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(2),
                    Val::Var(Identifier("tmp.3".to_string())),
                ),
                Instruction::Copy(
                    Val::Var(Identifier("tmp.3".to_string())),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Label(Identifier("continue.for.3".to_string())),
                Instruction::Copy(
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Var(Identifier("tmp.4".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Constant(1),
                    Val::Var(Identifier("var.b.2".to_string())),
                ),
                Instruction::Jump(Identifier("start.for.3".to_string())),
                Instruction::Label(Identifier("break.for.3".to_string())),
                tacky::Instruction::Return(Val::Var(Identifier("var.a.1".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn while_loop() {
    let result = tokenize_to_convert(
        " int main(void) { int a = 0; while(a < 10) { a++; if(a == 5) break; } return a; } ",
    );

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Copy(
                    Val::Constant(0),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Label(Identifier("continue.while.2".to_string())),
                Instruction::Binary(
                    BinaryOperator::LessThan,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(10),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::JumpIfZero(
                    Val::Var(Identifier("tmp.1".to_string())),
                    Identifier("break.while.2".to_string())
                ),
                Instruction::Copy(
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Var(Identifier("tmp.2".to_string()))
                ),
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(1),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::Equal,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(5),
                    Val::Var(Identifier("tmp.3".to_string())),
                ),
                Instruction::JumpIfZero(
                    Val::Var(Identifier("tmp.3".to_string())),
                    Identifier("endif.1".to_string())
                ),
                Instruction::Jump(Identifier("break.while.2".to_string())),
                Instruction::Label(Identifier("endif.1".to_string())),
                Instruction::Jump(Identifier("continue.while.2".to_string())),
                Instruction::Label(Identifier("break.while.2".to_string())),
                tacky::Instruction::Return(Val::Var(Identifier("var.a.1".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn do_while_loop() {
    let result =
        tokenize_to_convert(" int main(void) { int a = 0; do { a++; } while(a < 10); return a; } ");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Copy(
                    Val::Constant(0),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Label(Identifier("start.dowhile.2".to_string())),
                Instruction::Copy(
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(1),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Label(Identifier("continue.dowhile.2".to_string())),
                Instruction::Binary(
                    BinaryOperator::LessThan,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(10),
                    Val::Var(Identifier("tmp.2".to_string())),
                ),
                Instruction::JumpIfNotZero(
                    Val::Var(Identifier("tmp.2".to_string())),
                    Identifier("start.dowhile.2".to_string())
                ),
                Instruction::Label(Identifier("break.dowhile.2".to_string())),
                tacky::Instruction::Return(Val::Var(Identifier("var.a.1".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

#[test]
fn nested_while_loop() {
    let result = tokenize_to_convert(" int main(void) { int a = 0; while(a < 10) { int b = 0; while(b < 10) { b++; } a++; } return a; } ");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                Instruction::Copy(
                    Val::Constant(0),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Label(Identifier("continue.while.3".to_string())),
                Instruction::Binary(
                    BinaryOperator::LessThan,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(10),
                    Val::Var(Identifier("tmp.1".to_string())),
                ),
                Instruction::JumpIfZero(
                    Val::Var(Identifier("tmp.1".to_string())),
                    Identifier("break.while.3".to_string())
                ),
                Instruction::Copy(
                    Val::Constant(0),
                    Val::Var(Identifier("var.b.2".to_string())),
                ),
                Instruction::Label(Identifier("continue.while.4".to_string())),
                Instruction::Binary(
                    BinaryOperator::LessThan,
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Constant(10),
                    Val::Var(Identifier("tmp.2".to_string())),
                ),
                Instruction::JumpIfZero(
                    Val::Var(Identifier("tmp.2".to_string())),
                    Identifier("break.while.4".to_string())
                ),
                Instruction::Copy(
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Var(Identifier("tmp.3".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Var(Identifier("var.b.2".to_string())),
                    Val::Constant(1),
                    Val::Var(Identifier("var.b.2".to_string())),
                ),
                Instruction::Jump(Identifier("continue.while.4".to_string())),
                Instruction::Label(Identifier("break.while.4".to_string())),
                Instruction::Copy(
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Var(Identifier("tmp.4".to_string())),
                ),
                Instruction::Binary(
                    BinaryOperator::Add,
                    Val::Var(Identifier("var.a.1".to_string())),
                    Val::Constant(1),
                    Val::Var(Identifier("var.a.1".to_string())),
                ),
                Instruction::Jump(Identifier("continue.while.3".to_string())),
                Instruction::Label(Identifier("break.while.3".to_string())),
                tacky::Instruction::Return(Val::Var(Identifier("var.a.1".to_string()))),
            ]
        )),
        "{:#?}",
        result
    )
}

fn tokenize_to_convert(p: &str) -> tacky::Program {
    let mut result = token::tokenize(p.into()).unwrap();
    let mut result = parse(&mut result).unwrap();
    validate(&mut result).unwrap();

    convert(result).unwrap()
}
