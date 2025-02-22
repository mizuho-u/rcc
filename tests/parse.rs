use rc::parse::*;
use rc::token;

#[test]
fn valid_parse() {
    let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(
                Expression::Constant(1)
            ))]
        ))
    )
}

#[test]
fn valid_parse_unary() {
    let mut result = token::tokenize(" int main(void) { return ~(-1); } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Unary(
                UnaryOperator::Complement,
                Box::new(Expression::Unary(
                    UnaryOperator::Negate,
                    Box::new(Expression::Constant(1))
                ))
            )))]
        ))
    )
}

#[test]
fn valid_parse_binary1() {
    let mut result = token::tokenize(" int main(void) { return 1+2; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::Add,
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Constant(2)),
            )))]
        ))
    )
}

#[test]
fn valid_parse_binary2() {
    let mut result = token::tokenize(" int main(void) { return 1+2+3; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::Add,
                Box::new(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2)),
                )),
                Box::new(Expression::Constant(3)),
            )))]
        ))
    )
}

#[test]
fn valid_parse_binary3() {
    let mut result = token::tokenize(" int main(void) { return 1+(2+3); } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::Add,
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(3)),
                )),
            )))]
        ))
    )
}

#[test]
fn valid_parse_binary4() {
    let mut result = token::tokenize(" int main(void) { return 1+2*3; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::Add,
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(3)),
                )),
            )))]
        ))
    )
}

#[test]
fn valid_parse_binary5() {
    let mut result = token::tokenize(" int main(void) { return (1+2)*3; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::Multiply,
                Box::new(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2)),
                )),
                Box::new(Expression::Constant(3)),
            )))]
        ))
    )
}

#[test]
fn valid_parse_binary6() {
    let mut result = token::tokenize(" int main(void) { return (-1+2)*~3; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::Multiply,
                Box::new(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Unary(
                        UnaryOperator::Negate,
                        Box::new(Expression::Constant(1))
                    )),
                    Box::new(Expression::Constant(2)),
                )),
                Box::new(Expression::Unary(
                    UnaryOperator::Complement,
                    Box::new(Expression::Constant(3))
                )),
            )))]
        ))
    )
}

#[test]
fn valid_parse_bitwise_binary_operator1() {
    let mut result = token::tokenize(" int main(void) { return 3 & 1; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::And,
                Box::new(Expression::Constant(3)),
                Box::new(Expression::Constant(1)),
            )))]
        ))
    )
}

#[test]
fn valid_parse_bitwise_binary_operator2() {
    let mut result =
        token::tokenize(" int main(void) { return 1 | 2 ^ 3 & 4 << 5 >> 6; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::Or,
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Binary(
                    BinaryOperator::Xor,
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Binary(
                        BinaryOperator::And,
                        Box::new(Expression::Constant(3)),
                        Box::new(Expression::Binary(
                            BinaryOperator::RightShift,
                            Box::new(Expression::Binary(
                                BinaryOperator::LeftShit,
                                Box::new(Expression::Constant(4)),
                                Box::new(Expression::Constant(5)),
                            )),
                            Box::new(Expression::Constant(6)),
                        ))
                    )),
                )),
            )))]
        ))
    )
}

#[test]
fn valid_parse_bitwise_binary_operator3() {
    let mut result =
        token::tokenize(" int main(void) { return 1 << 2 - 3 * 4 ^ 5; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::Xor,
                Box::new(Expression::Binary(
                    BinaryOperator::LeftShit,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Binary(
                        BinaryOperator::Subtract,
                        Box::new(Expression::Constant(2)),
                        Box::new(Expression::Binary(
                            BinaryOperator::Multiply,
                            Box::new(Expression::Constant(3)),
                            Box::new(Expression::Constant(4)),
                        )),
                    )),
                )),
                Box::new(Expression::Constant(5)),
            )))]
        ))
    )
}

#[test]
fn logical_operator() {
    let mut result = token::tokenize(" int main(void) { return !1 && 2 || 3; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::LogicalOr,
                Box::new(Expression::Binary(
                    BinaryOperator::LogicalAnd,
                    Box::new(Expression::Unary(
                        UnaryOperator::Not,
                        Box::new(Expression::Constant(1))
                    )),
                    Box::new(Expression::Constant(2)),
                )),
                Box::new(Expression::Constant(3)),
            )))]
        ))
    )
}

#[test]
fn relational_operator() {
    //  (1 == 2) != ((((3 < 4) > 5) <= 6) >= 7)
    let mut result =
        token::tokenize(" int main(void) { return 1 == 2 != 3 < 4 > 5 <= 6 >= 7; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![BlockItem::Statement(Statement::Return(Expression::Binary(
                BinaryOperator::NotEqualTo,
                Box::new(Expression::Binary(
                    BinaryOperator::EqualTo,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2)),
                )),
                Box::new(Expression::Binary(
                    BinaryOperator::GreaterOrEqual,
                    Box::new(Expression::Binary(
                        BinaryOperator::LessOrEqual,
                        Box::new(Expression::Binary(
                            BinaryOperator::GreaterThan,
                            Box::new(Expression::Binary(
                                BinaryOperator::LessThan,
                                Box::new(Expression::Constant(3)),
                                Box::new(Expression::Constant(4)),
                            )),
                            Box::new(Expression::Constant(5)),
                        )),
                        Box::new(Expression::Constant(6)),
                    )),
                    Box::new(Expression::Constant(7)),
                )),
            )))]
        ))
    )
}

#[test]
fn parse_declaration() {
    let mut result = token::tokenize(" int main(void) { int a = 1; return a; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ]
        ))
    )
}

#[test]
fn parse_assignment() {
    let mut result =
        token::tokenize(" int main(void) { int a = 1; int b = 2; a = b; return a * b; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("b".to_string()),
                    Some(Expression::Constant(2))
                )),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("b".to_string()))),
                ))),
                BlockItem::Statement(Statement::Return(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("b".to_string()))),
                )))
            ]
        ))
    )
}

#[test]
fn parse_null_statement() {
    let mut result = token::tokenize(" int main(void) { int a = 1; ; return a; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Null),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ]
        ))
    )
}

#[test]
#[should_panic]
fn invalid_parse() {
    let mut result = token::tokenize(" int (void) { return 1; } ".into()).unwrap();
    parse(&mut result).unwrap();
}
