use crate::token::{self, Token};

#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Function),
}

#[derive(PartialEq, Debug)]
pub enum Function {
    Function(Identifier, Statement),
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOperator {
    Subtract,
    Add,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    Xor,
    LeftShit,
    RightShift,
    LogicalAnd,
    LogicalOr,
    EqualTo,
    NotEqualTo,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(PartialEq, Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct ParseError(String);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for ParseError {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl std::error::Error for ParseError {}

pub fn parse(tokens: &mut Vec<Token>) -> Result<Program, ParseError> {
    let p = parse_program(tokens)?;

    if tokens.len() >= 1 {
        Err(ParseError(format!("some tokens remaining {:?}", tokens)))
    } else {
        Ok(p)
    }
}

fn parse_program(tokens: &mut Vec<Token>) -> Result<Program, ParseError> {
    let f = parse_function(tokens)?;
    Ok(Program::Program(f))
}

fn parse_function(tokens: &mut Vec<Token>) -> Result<Function, ParseError> {
    expect(tokens, Token::Int)?;
    let id = parse_identifier(tokens)?;
    expect(tokens, Token::OpenParen)?;
    expect(tokens, Token::Void)?;
    expect(tokens, Token::CloseParen)?;
    expect(tokens, Token::OpenBrace)?;
    let s = parse_statement(tokens)?;
    expect(tokens, Token::CloseBrace)?;

    Ok(Function::Function(id, s))
}

fn parse_statement(tokens: &mut Vec<Token>) -> Result<Statement, ParseError> {
    expect(tokens, Token::Return)?;

    let e = parse_exp(tokens, 0)?;

    expect(tokens, Token::Semicolon)?;

    Ok(Statement::Return(e))
}

fn parse_factor(tokens: &mut Vec<Token>) -> Result<Expression, ParseError> {
    let next: &Token = peek(tokens);

    match *next {
        Token::Constant(n) => {
            consume(tokens);
            Ok(Expression::Constant(n))
        }
        Token::BitwiseComplementOperator => {
            consume(tokens);
            let exp = parse_factor(tokens)?;

            Ok(Expression::Unary(UnaryOperator::Complement, Box::new(exp)))
        }
        Token::NegationOperator => {
            consume(tokens);
            let exp = parse_factor(tokens)?;

            Ok(Expression::Unary(UnaryOperator::Negate, Box::new(exp)))
        }
        Token::LogicalNotOperator => {
            consume(tokens);
            let exp = parse_factor(tokens)?;

            Ok(Expression::Unary(UnaryOperator::Not, Box::new(exp)))
        }
        // Token::DecrementOperator => todo!(),
        Token::OpenParen => {
            consume(tokens);
            let exp = parse_exp(tokens, 0)?;
            expect(tokens, Token::CloseParen)?;
            Ok(exp)
        }
        _ => return Err(ParseError(format!("Malformed expression {:?}", next))),
    }
}

fn parse_exp(tokens: &mut Vec<Token>, min_prededence: i32) -> Result<Expression, ParseError> {
    let mut left = parse_factor(tokens)?;

    loop {
        let next = peek(tokens);

        if binop_precedence(next) < min_prededence {
            break;
        }

        match next {
            Token::NegationOperator
            | Token::AdditionOperator
            | Token::DivisionOperator
            | Token::MultiplicationOperator
            | Token::RemainderOperator
            | Token::AndOperator
            | Token::OrOperator
            | Token::XorOperator
            | Token::LeftShiftOperator
            | Token::RightShiftOperator
            | Token::LogicalAndOperator
            | Token::LogicalOrOperator
            | Token::EqualToOperator
            | Token::NotEqualToOperator
            | Token::LessThanOperator
            | Token::LessOrEqualOperator
            | Token::GreaterThanOperator
            | Token::GreaterOrEqualOperator => {
                let op = consume(tokens);
                let right = parse_exp(tokens, binop_precedence(&op) + 1)?;
                left = parse_binop(op, left, right)?;
            }
            _ => break,
        }
    }

    Ok(left)
}

fn parse_binop(op: Token, left: Expression, right: Expression) -> Result<Expression, ParseError> {
    let op = match op {
        Token::NegationOperator => Ok(BinaryOperator::Subtract),
        Token::AdditionOperator => Ok(BinaryOperator::Add),
        Token::DivisionOperator => Ok(BinaryOperator::Divide),
        Token::MultiplicationOperator => Ok(BinaryOperator::Multiply),
        Token::RemainderOperator => Ok(BinaryOperator::Remainder),
        Token::AndOperator => Ok(BinaryOperator::And),
        Token::OrOperator => Ok(BinaryOperator::Or),
        Token::XorOperator => Ok(BinaryOperator::Xor),
        Token::LeftShiftOperator => Ok(BinaryOperator::LeftShit),
        Token::RightShiftOperator => Ok(BinaryOperator::RightShift),
        Token::LogicalAndOperator => Ok(BinaryOperator::LogicalAnd),
        Token::LogicalOrOperator => Ok(BinaryOperator::LogicalOr),
        Token::EqualToOperator => Ok(BinaryOperator::EqualTo),
        Token::NotEqualToOperator => Ok(BinaryOperator::NotEqualTo),
        Token::LessThanOperator => Ok(BinaryOperator::LessThan),
        Token::LessOrEqualOperator => Ok(BinaryOperator::LessOrEqual),
        Token::GreaterThanOperator => Ok(BinaryOperator::GreaterThan),
        Token::GreaterOrEqualOperator => Ok(BinaryOperator::GreaterOrEqual),
        _ => Err(ParseError(format!("not binop"))),
    }?;

    Ok(Expression::Binary(op, Box::new(left), Box::new(right)))
}

fn binop_precedence(token: &Token) -> i32 {
    match token {
        Token::MultiplicationOperator | Token::DivisionOperator | Token::RemainderOperator => 100,
        Token::AdditionOperator | Token::NegationOperator => 90,
        Token::LeftShiftOperator | Token::RightShiftOperator => 80,
        Token::LessThanOperator
        | Token::LessOrEqualOperator
        | Token::GreaterThanOperator
        | Token::GreaterOrEqualOperator => 70,
        Token::EqualToOperator | Token::NotEqualToOperator => 60,
        Token::AndOperator => 50,
        Token::XorOperator => 40,
        Token::OrOperator => 30,
        Token::LogicalAndOperator => 29,
        Token::LogicalOrOperator => 28,
        _ => 0,
    }
}

fn parse_identifier(tokens: &mut Vec<Token>) -> Result<Identifier, ParseError> {
    let t = expect_fn(tokens, |t| matches!(t, Token::Identifier(_)))?;

    if let Token::Identifier(s) = t {
        return Ok(Identifier(s));
    }

    Err(ParseError(format!("Invalid Identifier {:?}", t)))
}
fn expect(tokens: &mut Vec<Token>, expected: token::Token) -> Result<Token, ParseError> {
    let head = consume(tokens);

    if expected != head {
        return Err(ParseError(format!(
            "expected {:?} got {:?}",
            expected, head
        )));
    }

    Ok(head)
}

fn expect_fn(tokens: &mut Vec<Token>, cb: fn(&Token) -> bool) -> Result<Token, ParseError> {
    let head = consume(tokens);

    if !cb(&head) {
        return Err(ParseError(format!("unexpected token {:?}", head)));
    }

    Ok(head)
}

fn peek(tokens: &Vec<Token>) -> &Token {
    &tokens[0]
}

fn consume(tokens: &mut Vec<Token>) -> Token {
    tokens.remove(0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;
    use crate::token;

    #[test]
    fn valid_parse() {
        let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        assert_eq!(
            result,
            Program::Program(Function::Function(
                Identifier("main".to_string()),
                Statement::Return(Expression::Constant(1))
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
                Statement::Return(Expression::Unary(
                    UnaryOperator::Complement,
                    Box::new(Expression::Unary(
                        UnaryOperator::Negate,
                        Box::new(Expression::Constant(1))
                    ))
                ))
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
                Statement::Return(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2)),
                ))
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
                Statement::Return(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(Expression::Constant(1)),
                        Box::new(Expression::Constant(2)),
                    )),
                    Box::new(Expression::Constant(3)),
                ))
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
                Statement::Return(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(Expression::Constant(2)),
                        Box::new(Expression::Constant(3)),
                    )),
                ))
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
                Statement::Return(Expression::Binary(
                    BinaryOperator::Add,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Binary(
                        BinaryOperator::Multiply,
                        Box::new(Expression::Constant(2)),
                        Box::new(Expression::Constant(3)),
                    )),
                ))
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
                Statement::Return(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(Expression::Constant(1)),
                        Box::new(Expression::Constant(2)),
                    )),
                    Box::new(Expression::Constant(3)),
                ))
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
                Statement::Return(Expression::Binary(
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
                ))
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
                Statement::Return(Expression::Binary(
                    BinaryOperator::And,
                    Box::new(Expression::Constant(3)),
                    Box::new(Expression::Constant(1)),
                ))
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
                Statement::Return(Expression::Binary(
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
                ))
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
                Statement::Return(Expression::Binary(
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
                ))
            ))
        )
    }

    #[test]
    fn logical_operator() {
        let mut result =
            token::tokenize(" int main(void) { return !1 && 2 || 3; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        assert_eq!(
            result,
            Program::Program(Function::Function(
                Identifier("main".to_string()),
                Statement::Return(Expression::Binary(
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
                ))
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
                Statement::Return(Expression::Binary(
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
                ))
            ))
        )
    }

    #[test]
    #[should_panic]
    fn invalid_parse() {
        let mut result = token::tokenize(" int (void) { return 1; } ".into()).unwrap();
        parse(&mut result).unwrap();
    }
}
