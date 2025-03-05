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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Constant(1)
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Unary(
                    UnaryOperator::Complement,
                    Box::new(Expression::Unary(
                        UnaryOperator::Negate,
                        Box::new(Expression::Constant(1))
                    ))
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2)),
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(Expression::Constant(1)),
                        Box::new(Expression::Constant(2)),
                    )),
                    Box::new(Expression::Constant(3)),
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(Expression::Constant(2)),
                        Box::new(Expression::Constant(3)),
                    )),
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Binary(
                        BinaryOperator::Multiply,
                        Box::new(Expression::Constant(2)),
                        Box::new(Expression::Constant(3)),
                    )),
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(Expression::Constant(1)),
                        Box::new(Expression::Constant(2)),
                    )),
                    Box::new(Expression::Constant(3)),
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
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
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
                    BinaryOperator::And,
                    Box::new(Expression::Constant(3)),
                    Box::new(Expression::Constant(1)),
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
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
                                    BinaryOperator::LeftShift,
                                    Box::new(Expression::Constant(4)),
                                    Box::new(Expression::Constant(5)),
                                )),
                                Box::new(Expression::Constant(6)),
                            ))
                        )),
                    )),
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
                    BinaryOperator::Xor,
                    Box::new(Expression::Binary(
                        BinaryOperator::LeftShift,
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
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
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
                )
            ))])
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
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Binary(
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
                )
            ))])
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
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
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
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("b".to_string()),
                    Some(Expression::Constant(2))
                )),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::Simple,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("b".to_string()))),
                ))),
                BlockItem::Statement(Statement::Return(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("b".to_string()))),
                )))
            ])
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
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Null),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        ))
    )
}

#[test]
fn increment1() {
    let mut result =
        token::tokenize(" int main(void) { int a = 1; ++a; return a; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Expression(Expression::Unary(
                    UnaryOperator::IncrementPrefix,
                    Box::new(Expression::Var(Identifier("a".to_string())))
                ))),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        ))
    )
}

#[test]
fn increment2() {
    let mut result =
        token::tokenize(" int main(void) { int a = 1; a++; return a; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Expression(Expression::Unary(
                    UnaryOperator::IncrementPostfix,
                    Box::new(Expression::Var(Identifier("a".to_string())))
                ))),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        ))
    )
}

#[test]
fn increment3() {
    let mut result =
        token::tokenize(" int main(void) { int a = 1; -a++; return a; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Expression(Expression::Unary(
                    UnaryOperator::Negate,
                    Box::new(Expression::Unary(
                        UnaryOperator::IncrementPostfix,
                        Box::new(Expression::Var(Identifier("a".to_string())))
                    ))
                ))),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        ))
    )
}

#[test]
fn increment4() {
    let mut result =
        token::tokenize(" int main(void) { int a = 10; return a++--; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(10))
                )),
                BlockItem::Statement(Statement::Return(Expression::Unary(
                    UnaryOperator::DecrementPostfix,
                    Box::new(Expression::Unary(
                        UnaryOperator::IncrementPostfix,
                        Box::new(Expression::Var(Identifier("a".to_string())))
                    ))
                ))),
            ])
        ))
    )
}

#[test]
fn compound() {
    let mut result = token::tokenize(
        " int main(void) { int a = 1; int b = 2; int c = 3; a += 10; a -= 1; a *= a; a /= b; a %= c; a &= 1; a |= a; a ^= b; a <<= c; a >>= 1; return a; } ".into(),
    )
    .unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("b".to_string()),
                    Some(Expression::Constant(2))
                )),
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("c".to_string()),
                    Some(Expression::Constant(3))
                )),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::Addition,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Constant(10)),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::Subtract,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Constant(1)),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::Multiplication,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::Division,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("b".to_string()))),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::Remainder,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("c".to_string()))),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::And,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Constant(1)),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::Or,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::Xor,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("b".to_string()))),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::LeftShift,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Var(Identifier("c".to_string()))),
                ))),
                BlockItem::Statement(Statement::Expression(Expression::Assignment(
                    AssignmentOperator::RightShift,
                    Box::new(Expression::Var(Identifier("a".to_string()))),
                    Box::new(Expression::Constant(1)),
                ))),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                )))),
            ])
        ))
    )
}

#[test]
fn if_statement() {
    let mut result =
        token::tokenize(" int main(void) { if (1) return 1; return 2; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Statement(Statement::If(
                    Expression::Constant(1),
                    Box::new(Statement::Return(Expression::Constant(1))),
                    None
                )),
                BlockItem::Statement(Statement::Return(Expression::Constant(2))),
            ])
        ))
    )
}

#[test]
fn if_else_statement() {
    let mut result =
        token::tokenize(" int main(void) { if (1) return 1; else return 2; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![BlockItem::Statement(Statement::If(
                Expression::Constant(1),
                Box::new(Statement::Return(Expression::Constant(1))),
                Some(Box::new(Statement::Return(Expression::Constant(2)))),
            )),])
        ))
    )
}

#[test]
fn conditional_expression() {
    let mut result = token::tokenize(" int main(void) { return 1 ? 10 : 20; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Conditional(
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(10)),
                    Box::new(Expression::Constant(20))
                ),
            )),])
        ))
    )
}

#[test]
fn nested_conditional_expression() {
    let mut result =
        token::tokenize(" int main(void) { return 1 ? 2 ? 10 : 20 : 30; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();
    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![BlockItem::Statement(Statement::Return(
                Expression::Conditional(
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Conditional(
                        Box::new(Expression::Constant(2)),
                        Box::new(Expression::Constant(10)),
                        Box::new(Expression::Constant(20)),
                    )),
                    Box::new(Expression::Constant(30))
                ),
            )),])
        ))
    )
}

#[test]
#[should_panic]
fn invalid_parse() {
    let mut result = token::tokenize(" int (void) { return 1; } ".into()).unwrap();
    parse(&mut result).unwrap();
}

#[test]
fn prefix_conditional() {
    let mut result =
        token::tokenize(" int main(void) { int a = 0; return (++a ? ++a : 0); } ".into()).unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::Return(Expression::Conditional(
                    Box::new(Expression::Unary(
                        UnaryOperator::IncrementPrefix,
                        Box::new(Expression::Var(Identifier("a".to_string())))
                    )),
                    Box::new(Expression::Unary(
                        UnaryOperator::IncrementPrefix,
                        Box::new(Expression::Var(Identifier("a".to_string())))
                    )),
                    Box::new(Expression::Constant(0))
                )))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn goto_statement() {
    let mut result = token::tokenize(
        " int main(void) { int a = 0; goto label; int b = 1; label: b = 2; return b; } ".into(),
    )
    .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::Goto(Identifier("label".to_string()))),
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("b".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Label(
                    Identifier("label".to_string()),
                    Box::new(Statement::Expression(Expression::Assignment(
                        AssignmentOperator::Simple,
                        Box::new(Expression::Var(Identifier("b".to_string()))),
                        Box::new(Expression::Constant(2)),
                    ))),
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "b".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn compound_statement() {
    let mut result =
        token::tokenize(" int main(void) { int a = 0; { int b = 1; b = 2; } return a; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::Compound(Block::Block(vec![
                    BlockItem::Declaration(Declaration::Declaration(
                        Identifier("b".to_string()),
                        Some(Expression::Constant(1))
                    )),
                    BlockItem::Statement(Statement::Expression(Expression::Assignment(
                        AssignmentOperator::Simple,
                        Box::new(Expression::Var(Identifier("b".to_string()))),
                        Box::new(Expression::Constant(2)),
                    ))),
                ]))),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn compound_if_statement() {
    let mut result = token::tokenize(
        " int main(void) { int a = 0; if (0) { int b = 1; b = 2; } else { a = 3; } return a; } "
            .into(),
    )
    .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::If(
                    Expression::Constant(0),
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Declaration(Declaration::Declaration(
                            Identifier("b".to_string()),
                            Some(Expression::Constant(1))
                        )),
                        BlockItem::Statement(Statement::Expression(Expression::Assignment(
                            AssignmentOperator::Simple,
                            Box::new(Expression::Var(Identifier("b".to_string()))),
                            Box::new(Expression::Constant(2)),
                        ))),
                    ]))),
                    Some(Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::Expression(Expression::Assignment(
                            AssignmentOperator::Simple,
                            Box::new(Expression::Var(Identifier("a".to_string()))),
                            Box::new(Expression::Constant(3)),
                        ))),
                    ]))))
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn while_statement() {
    let mut result =
        token::tokenize(" int main(void) { int a = 0; while(a < 10) { a++; } return a; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::While(
                    Expression::Binary(
                        BinaryOperator::LessThan,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                        Box::new(Expression::Constant(10))
                    ),
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::Expression(Expression::Unary(
                            UnaryOperator::IncrementPostfix,
                            Box::new(Expression::Var(Identifier("a".to_string()))),
                        )))
                    ]))),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn while_no_statement() {
    let mut result =
        token::tokenize(" int main(void) { int a = 0; while(a < 10) ; return a; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::While(
                    Expression::Binary(
                        BinaryOperator::LessThan,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                        Box::new(Expression::Constant(10))
                    ),
                    Box::new(Statement::Null),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn do_while() {
    let mut result =
        token::tokenize(" int main(void) { int a = 0; do a++; while(a < 10); return a; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::DoWhile(
                    Box::new(Statement::Expression(Expression::Unary(
                        UnaryOperator::IncrementPostfix,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                    ))),
                    Expression::Binary(
                        BinaryOperator::LessThan,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                        Box::new(Expression::Constant(10))
                    ),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn do_block_while() {
    let mut result = token::tokenize(
        " int main(void) { int a = 0; do { a++; } while(a < 10); return a; } ".into(),
    )
    .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::DoWhile(
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::Expression(Expression::Unary(
                            UnaryOperator::IncrementPostfix,
                            Box::new(Expression::Var(Identifier("a".to_string()))),
                        ))),
                    ]))),
                    Expression::Binary(
                        BinaryOperator::LessThan,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                        Box::new(Expression::Constant(10))
                    ),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn for_init_declaration() {
    let mut result =
        token::tokenize(" int main(void) { for(int a = 0; a < 10; a++) ; return a; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Statement(Statement::For(
                    ForInit::Declaration(Declaration::Declaration(
                        Identifier("a".to_string()),
                        Some(Expression::Constant(0))
                    )),
                    Some(Expression::Binary(
                        BinaryOperator::LessThan,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                        Box::new(Expression::Constant(10))
                    )),
                    Some(Expression::Unary(
                        UnaryOperator::IncrementPostfix,
                        Box::new(Expression::Var(Identifier("a".to_string())))
                    )),
                    Box::new(Statement::Null),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn for_init_expression() {
    let mut result = token::tokenize(
        " int main(void) { int a = 1; for(a = 0; a < 10; a++) ; return a; } ".into(),
    )
    .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::For(
                    ForInit::Expression(Some(Expression::Assignment(
                        AssignmentOperator::Simple,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                        Box::new(Expression::Constant(0))
                    ))),
                    Some(Expression::Binary(
                        BinaryOperator::LessThan,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                        Box::new(Expression::Constant(10))
                    )),
                    Some(Expression::Unary(
                        UnaryOperator::IncrementPostfix,
                        Box::new(Expression::Var(Identifier("a".to_string())))
                    )),
                    Box::new(Statement::Null),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn for_no_init_cond_post() {
    let mut result =
        token::tokenize(" int main(void) { for(;;) break; return a; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Statement(Statement::For(
                    ForInit::Expression(None),
                    None,
                    None,
                    Box::new(Statement::Break(Identifier::placeholder())),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn for_block() {
    let mut result =
        token::tokenize(" int main(void) { int a = 0; for(;a < 10;) { a++; } return a; } ".into())
            .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::For(
                    ForInit::Expression(None),
                    Some(Expression::Binary(
                        BinaryOperator::LessThan,
                        Box::new(Expression::Var(Identifier("a".to_string()))),
                        Box::new(Expression::Constant(10))
                    )),
                    None,
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::Expression(Expression::Unary(
                            UnaryOperator::IncrementPostfix,
                            Box::new(Expression::Var(Identifier("a".to_string())))
                        )))
                    ]))),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn for_continue_break() {
    let mut result = token::tokenize(
        " int main(void) { int a = 0; for(;;a++) { if(a%2 == 0) continue; if(a == 10) break; } return a; } ".into(),
    )
    .unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::For(
                    ForInit::Expression(None),
                    None,
                    Some(Expression::Unary(
                        UnaryOperator::IncrementPostfix,
                        Box::new(Expression::Var(Identifier("a".to_string())))
                    )),
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::If(
                            Expression::Binary(
                                BinaryOperator::EqualTo,
                                Box::new(Expression::Binary(
                                    BinaryOperator::Remainder,
                                    Box::new(Expression::Var(Identifier("a".to_string()))),
                                    Box::new(Expression::Constant(2))
                                )),
                                Box::new(Expression::Constant(0))
                            ),
                            Box::new(Statement::Continue(Identifier::placeholder())),
                            None
                        )),
                        BlockItem::Statement(Statement::If(
                            Expression::Binary(
                                BinaryOperator::EqualTo,
                                Box::new(Expression::Var(Identifier("a".to_string()))),
                                Box::new(Expression::Constant(10))
                            ),
                            Box::new(Statement::Break(Identifier::placeholder())),
                            None
                        ))
                    ]))),
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn switch_case() {
    let mut result = token::tokenize(" int main(void) { int a = 0; switch(a) { int b = 0; case 0: a += 0; break; case 1: a += 1; default: a+=2; } return a; } ".into()).unwrap();
    let result = parse(&mut result).unwrap();

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("a".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::Switch(
                    Expression::Var(Identifier("a".to_string())),
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Declaration(Declaration::Declaration(
                            Identifier("b".to_string()),
                            Some(Expression::Constant(0))
                        )),
                        BlockItem::Statement(Statement::Case(
                            Expression::Constant(0),
                            Some(Box::new(Statement::Expression(Expression::Assignment(
                                AssignmentOperator::Addition,
                                Box::new(Expression::Var(Identifier("a".to_string()))),
                                Box::new(Expression::Constant(0))
                            )))),
                            Identifier::placeholder()
                        )),
                        BlockItem::Statement(Statement::Break(Identifier::placeholder())),
                        BlockItem::Statement(Statement::Case(
                            Expression::Constant(1),
                            Some(Box::new(Statement::Expression(Expression::Assignment(
                                AssignmentOperator::Addition,
                                Box::new(Expression::Var(Identifier("a".to_string()))),
                                Box::new(Expression::Constant(1))
                            )))),
                            Identifier::placeholder()
                        )),
                        BlockItem::Statement(Statement::Default(
                            Some(Box::new(Statement::Expression(Expression::Assignment(
                                AssignmentOperator::Addition,
                                Box::new(Expression::Var(Identifier("a".to_string()))),
                                Box::new(Expression::Constant(2))
                            )))),
                            Identifier::placeholder()
                        )),
                    ]))),
                    vec![],
                    Identifier::placeholder()
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "a".to_string()
                ))))
            ])
        )),
        "{:#?}",
        result
    )
}
