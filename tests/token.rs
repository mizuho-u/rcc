use rc::token;
use rc::token::Token;

#[test]
fn valid_token() {
    let result = token::tokenize(" int main(void) { return -(~~(++(--1))); } ".into()).unwrap();
    assert_eq!(
        result,
        vec![
            Token::Int,
            Token::Identifier("main".to_string()),
            Token::OpenParen,
            Token::Void,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Return,
            Token::NegationOperator,
            Token::OpenParen,
            Token::BitwiseComplementOperator,
            Token::BitwiseComplementOperator,
            Token::OpenParen,
            Token::IncrementOperator,
            Token::OpenParen,
            Token::DecrementOperator,
            Token::Constant(1),
            Token::CloseParen,
            Token::CloseParen,
            Token::CloseParen,
            Token::Semicolon,
            Token::CloseBrace,
        ]
    )
}

#[test]
fn binop() {
    let result =
        token::tokenize(" int main(void) { return 1-(2+(3*(4/(5%6)))); } ".into()).unwrap();
    assert_eq!(
        result,
        vec![
            Token::Int,
            Token::Identifier("main".to_string()),
            Token::OpenParen,
            Token::Void,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Return,
            Token::Constant(1),
            Token::NegationOperator,
            Token::OpenParen,
            Token::Constant(2),
            Token::AdditionOperator,
            Token::OpenParen,
            Token::Constant(3),
            Token::MultiplicationOperator,
            Token::OpenParen,
            Token::Constant(4),
            Token::DivisionOperator,
            Token::OpenParen,
            Token::Constant(5),
            Token::RemainderOperator,
            Token::Constant(6),
            Token::CloseParen,
            Token::CloseParen,
            Token::CloseParen,
            Token::CloseParen,
            Token::Semicolon,
            Token::CloseBrace,
        ]
    )
}

#[test]
fn bitwise_operator() {
    let result = token::tokenize(" int main(void) { return 1<<(2|3)>>4&5^6; } ".into()).unwrap();
    assert_eq!(
        result,
        vec![
            Token::Int,
            Token::Identifier("main".to_string()),
            Token::OpenParen,
            Token::Void,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Return,
            Token::Constant(1),
            Token::LeftShiftOperator,
            Token::OpenParen,
            Token::Constant(2),
            Token::OrOperator,
            Token::Constant(3),
            Token::CloseParen,
            Token::RightShiftOperator,
            Token::Constant(4),
            Token::AndOperator,
            Token::Constant(5),
            Token::XorOperator,
            Token::Constant(6),
            Token::Semicolon,
            Token::CloseBrace,
        ]
    )
}

#[test]
fn logical_operator() {
    let result = token::tokenize(" int main(void) { return !1 && 2 || 3; } ".into()).unwrap();
    assert_eq!(
        result,
        vec![
            Token::Int,
            Token::Identifier("main".to_string()),
            Token::OpenParen,
            Token::Void,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Return,
            Token::LogicalNotOperator,
            Token::Constant(1),
            Token::LogicalAndOperator,
            Token::Constant(2),
            Token::LogicalOrOperator,
            Token::Constant(3),
            Token::Semicolon,
            Token::CloseBrace,
        ]
    )
}

#[test]
fn relational_operator() {
    let result =
        token::tokenize(" int main(void) { return 1 == 2 != 3 < 4 > 5 <= 6 >= 7; } ".into())
            .unwrap();
    assert_eq!(
        result,
        vec![
            Token::Int,
            Token::Identifier("main".to_string()),
            Token::OpenParen,
            Token::Void,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Return,
            Token::Constant(1),
            Token::EqualToOperator,
            Token::Constant(2),
            Token::NotEqualToOperator,
            Token::Constant(3),
            Token::LessThanOperator,
            Token::Constant(4),
            Token::GreaterThanOperator,
            Token::Constant(5),
            Token::LessOrEqualOperator,
            Token::Constant(6),
            Token::GreaterOrEqualOperator,
            Token::Constant(7),
            Token::Semicolon,
            Token::CloseBrace,
        ]
    )
}

#[test]
fn assignment() {
    let result = token::tokenize(" int main(void) { int a = 1; return a; } ".into()).unwrap();
    assert_eq!(
        result,
        vec![
            Token::Int,
            Token::Identifier("main".to_string()),
            Token::OpenParen,
            Token::Void,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Int,
            Token::Identifier("a".to_string()),
            Token::AssignmentOperator,
            Token::Constant(1),
            Token::Semicolon,
            Token::Return,
            Token::Identifier("a".to_string()),
            Token::Semicolon,
            Token::CloseBrace,
        ]
    )
}

#[test]
fn compound_assignment() {
    let result = token::tokenize(" += -= *= /= %= &= |= ^= <<= >>= ".into()).unwrap();
    assert_eq!(
        result,
        vec![
            Token::CompoundAssignmentAdditionOperator,
            Token::CompoundAssignmentSubtractOperator,
            Token::CompoundAssignmentMultiplicationOperator,
            Token::CompoundAssignmentDivisionOperator,
            Token::CompoundAssignmentRemainderOperator,
            Token::CompoundAssignmentAndOperator,
            Token::CompoundAssignmentOrOperator,
            Token::CompoundAssignmentXorOperator,
            Token::CompoundAssignmentLeftShiftOperator,
            Token::CompoundAssignmentRightShiftOperator,
        ]
    )
}

#[test]
fn if_statements() {
    let result = token::tokenize(" if else ? : ".into()).unwrap();
    assert_eq!(
        result,
        vec![Token::If, Token::Else, Token::QuestionMark, Token::Colon,]
    )
}

#[test]
fn goto() {
    let result = token::tokenize(" goto ".into()).unwrap();
    assert_eq!(result, vec![Token::Goto])
}

#[test]
#[should_panic]
fn invalid_token() {
    token::tokenize(" 1foo ".into()).unwrap();
}
