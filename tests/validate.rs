use rc::parse::*;
use rc::token;

#[test]
fn no_resolution() {
    let result = tokenize_to_validate(" int main(void) { return 1; } ");

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
fn var_resolution() {
    let result = tokenize_to_validate(" int main(void) { int a = 1; return a; } ");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("var.a.1".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "var.a.1".to_string()
                ))))
            ])
        ))
    )
}

#[test]
fn using_variables_in_their_own_initializers() {
    let result = tokenize_to_validate(" int main(void) { int a = a + 1; return a; }");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("var.a.1".to_string()),
                    Some(Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(Expression::Var(Identifier("var.a.1".to_string()))),
                        Box::new(Expression::Constant(1)),
                    ))
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "var.a.1".to_string()
                ))))
            ])
        ))
    )
}

#[should_panic]
#[test]
fn mixed_precedence_assignment() {
    let result = tokenize_to_validate(" int main(void) { int a = 1; int b = 2; a = 3 * b = a; ");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("var.a.1".to_string()),
                    Some(Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(Expression::Var(Identifier("var.a.1".to_string()))),
                        Box::new(Expression::Constant(1)),
                    ))
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "var.a.1".to_string()
                ))))
            ])
        ),),
        "{:#?}",
        result
    )
}

#[should_panic]
#[test]
fn duplicate_declaration() {
    tokenize_to_validate(" int main(void) { int a = 1; int a = 2; return a; ");
}

#[should_panic]
#[test]
fn invalid_lvalue_assignment() {
    tokenize_to_validate(" int main(void) { 1 = 2; return 1; ");
}

#[should_panic]
#[test]
fn undeclared_value() {
    tokenize_to_validate("int main(void) { return a; }");
}

#[should_panic]
#[test]
fn duplicate_goto_label() {
    tokenize_to_validate(" int main(void) { int a = 1; label_a: a = 2; label_a: return a; } ");
}

#[should_panic]
#[test]
fn goto_undefined_label() {
    tokenize_to_validate(
        " int main(void) { goto label_b; int a = 1; label_a: int b = 2; return a; } ",
    );
}

#[test]
fn same_var_name_in_different_block() {
    tokenize_to_validate(" int main(void) { int a = 0; { int a = 0; } return a; } ");
}

#[should_panic]
#[test]
fn same_var_name_in_same_block() {
    tokenize_to_validate(" int main(void) { int a; int a; return a; } ");
}

#[test]
fn for_loop_labeling() {
    let result = tokenize_to_validate(" int main(void) { for(;;) break; return 1; }");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Statement(Statement::For(
                    ForInit::Expression(None),
                    None,
                    None,
                    Box::new(Statement::Break(Identifier("for.1".to_string()))),
                    Identifier("for.1".to_string())
                )),
                BlockItem::Statement(Statement::Return(Expression::Constant(1)))
            ])
        ))
    )
}

#[test]
fn while_loop_labeling() {
    let result = tokenize_to_validate(" int main(void) { while(1) continue; return 1; }");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Statement(Statement::While(
                    Expression::Constant(1),
                    Box::new(Statement::Continue(Identifier("while.1".to_string()))),
                    Identifier("while.1".to_string())
                )),
                BlockItem::Statement(Statement::Return(Expression::Constant(1)))
            ])
        ))
    )
}

#[test]
fn do_while_loop_labeling() {
    let result = tokenize_to_validate(" int main(void) { int a = 0; do { if(a == 2) continue; if(a == 5) break; } while(a < 10); return 1; }");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("var.a.1".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::DoWhile(
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::If(
                            Expression::Binary(
                                BinaryOperator::EqualTo,
                                Box::new(Expression::Var(Identifier("var.a.1".to_string()))),
                                Box::new(Expression::Constant(2))
                            ),
                            Box::new(Statement::Continue(Identifier("dowhile.2".to_string()))),
                            None
                        )),
                        BlockItem::Statement(Statement::If(
                            Expression::Binary(
                                BinaryOperator::EqualTo,
                                Box::new(Expression::Var(Identifier("var.a.1".to_string()))),
                                Box::new(Expression::Constant(5))
                            ),
                            Box::new(Statement::Break(Identifier("dowhile.2".to_string()))),
                            None
                        ))
                    ]))),
                    Expression::Binary(
                        BinaryOperator::LessThan,
                        Box::new(Expression::Var(Identifier("var.a.1".to_string()))),
                        Box::new(Expression::Constant(10))
                    ),
                    Identifier("dowhile.2".to_string())
                )),
                BlockItem::Statement(Statement::Return(Expression::Constant(1)))
            ])
        ))
    )
}

#[test]
fn nested_loop_labeling() {
    let result = tokenize_to_validate(
        " int main(void) { for(;;) { while(0) { continue; } break; } return 1; }",
    );

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Statement(Statement::For(
                    ForInit::Expression(None),
                    None,
                    None,
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::While(
                            Expression::Constant(0),
                            Box::new(Statement::Compound(Block::Block(vec![
                                BlockItem::Statement(Statement::Continue(Identifier(
                                    "while.2".to_string()
                                ))),
                            ]))),
                            Identifier("while.2".to_string())
                        )),
                        BlockItem::Statement(Statement::Break(Identifier("for.1".to_string())))
                    ]))),
                    Identifier("for.1".to_string())
                )),
                BlockItem::Statement(Statement::Return(Expression::Constant(1)))
            ])
        ))
    )
}

#[test]
fn switch_labeling() {
    let result = tokenize_to_validate(
        " int main(void) { int a = 0; switch(0) { case 0: a += 1; case 1: break; default: } return 1; }",
    );

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("var.a.1".to_string()),
                    Some(Expression::Constant(0))
                )),
                BlockItem::Statement(Statement::Switch(
                    Expression::Constant(0),
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::Case(
                            Expression::Constant(0),
                            Some(Box::new(Statement::Expression(Expression::Assignment(
                                AssignmentOperator::Addition,
                                Box::new(Expression::Var(Identifier("var.a.1".to_string()))),
                                Box::new(Expression::Constant(1))
                            )))),
                            Identifier("switch.2.3".to_string()),
                        )),
                        BlockItem::Statement(Statement::Case(
                            Expression::Constant(1),
                            Some(Box::new(Statement::Break(Identifier(
                                "switch.2".to_string()
                            )))),
                            Identifier("switch.2.4".to_string()),
                        )),
                        BlockItem::Statement(Statement::Default(
                            None,
                            Identifier("switch.2.5".to_string()),
                        )),
                    ]))),
                    vec![
                        (
                            Some(Expression::Constant(0)),
                            Identifier("switch.2.3".to_string())
                        ),
                        (
                            Some(Expression::Constant(1)),
                            Identifier("switch.2.4".to_string())
                        ),
                        (None, Identifier("switch.2.5".to_string())),
                    ],
                    Identifier("switch.2".to_string())
                )),
                BlockItem::Statement(Statement::Return(Expression::Constant(1)))
            ])
        )),
        "{:#?}",
        result
    )
}

#[test]
fn switch_inside_loop_labeling() {
    let result = tokenize_to_validate(
        " int main(void) { for(;;) { switch(0) { case 0: continue; case 1: break; } break; } return 1; }",
    );

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            Block::Block(vec![
                BlockItem::Statement(Statement::For(
                    ForInit::Expression(None),
                    None,
                    None,
                    Box::new(Statement::Compound(Block::Block(vec![
                        BlockItem::Statement(Statement::Switch(
                            Expression::Constant(0),
                            Box::new(Statement::Compound(Block::Block(vec![
                                BlockItem::Statement(Statement::Case(
                                    Expression::Constant(0),
                                    Some(Box::new(Statement::Continue(Identifier(
                                        "for.1".to_string()
                                    )))),
                                    Identifier("switch.2.3".to_string()),
                                )),
                                BlockItem::Statement(Statement::Case(
                                    Expression::Constant(1),
                                    Some(Box::new(Statement::Break(Identifier(
                                        "switch.2".to_string()
                                    )))),
                                    Identifier("switch.2.4".to_string()),
                                )),
                            ]))),
                            vec![
                                (
                                    Some(Expression::Constant(0)),
                                    Identifier("switch.2.3".to_string())
                                ),
                                (
                                    Some(Expression::Constant(1)),
                                    Identifier("switch.2.4".to_string())
                                )
                            ],
                            Identifier("switch.2".to_string())
                        )),
                        BlockItem::Statement(Statement::Break(Identifier("for.1".to_string())))
                    ]))),
                    Identifier("for.1".to_string())
                )),
                BlockItem::Statement(Statement::Return(Expression::Constant(1)))
            ])
        )),
        "{:#?}",
        result
    )
}

#[should_panic]
#[test]
fn break_statement_outside_loop() {
    tokenize_to_validate(" int main(void) { break; return 1; } ");
}

#[should_panic]
#[test]
fn continue_statement_outside_loop() {
    tokenize_to_validate(" int main(void) { continue; while(0); return 1; } ");
}

#[should_panic]
#[test]
fn continue_statement_inside_switch() {
    tokenize_to_validate(" int main(void) { switch(0) { case 0: continue; } return 1; } ");
}

#[should_panic]
#[test]
fn duplicate_cases_switch() {
    tokenize_to_validate(
        " int main(void) { int a = 0; switch(0) { case 0: a++; case 0: break; default: } return 1; } ",
    );
}

#[should_panic]
#[test]
fn duplicate_defaults_switch() {
    tokenize_to_validate(
        " int main(void) { int a = 0; switch(0) { case 0: break; default: a++; default: break; } return 1; } ",
    );
}

fn tokenize_to_validate(p: &str) -> Program {
    let mut result = token::tokenize(p.into()).unwrap();
    let mut result = parse(&mut result).unwrap();
    validate(&mut result).unwrap();

    result
}
