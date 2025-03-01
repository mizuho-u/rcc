use rc::parse::*;
use rc::token;

#[test]
fn no_resolution() {
    let result = tokenize_to_validate(" int main(void) { return 1; } ");

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
fn var_resolution() {
    let result = tokenize_to_validate(" int main(void) { int a = 1; return a; } ");

    assert_eq!(
        result,
        Program::Program(Function::Function(
            Identifier("main".to_string()),
            vec![
                BlockItem::Declaration(Declaration::Declaration(
                    Identifier("var.a.1".to_string()),
                    Some(Expression::Constant(1))
                )),
                BlockItem::Statement(Statement::Return(Expression::Var(Identifier(
                    "var.a.1".to_string()
                ))))
            ]
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
            vec![
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
            ]
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
            vec![
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
            ]
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

fn tokenize_to_validate(p: &str) -> Program {
    let mut result = token::tokenize(p.into()).unwrap();
    let result = parse(&mut result).unwrap();
    validate(result).unwrap()
}

#[should_panic]
#[test]
fn duplicate_goto_label() {
    let res =
        tokenize_to_validate(" int main(void) { int a = 1; label_a: a = 2; label_a: return a; } ");
    dbg!(res);
}

#[should_panic]
#[test]
fn goto_undefined_label() {
    tokenize_to_validate(
        " int main(void) { goto label_b; int a = 1; label_a: int b = 2; return a; } ",
    );
}
