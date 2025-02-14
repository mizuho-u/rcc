use crate::{
    ast::{self, Expression},
    token::{self, Token},
};

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

pub fn parse(tokens: &mut Vec<Token>) -> Result<ast::Program, ParseError> {
    let p = parse_program(tokens)?;

    if tokens.len() >= 1 {
        Err(ParseError(format!("some tokens remaining {:?}", tokens)))
    } else {
        Ok(p)
    }
}

fn parse_program(tokens: &mut Vec<Token>) -> Result<ast::Program, ParseError> {
    let f = parse_function(tokens)?;
    Ok(ast::Program::Program(f))
}

fn parse_function(tokens: &mut Vec<Token>) -> Result<ast::Function, ParseError> {
    expect(tokens, Token::Int)?;
    let id = parse_identifier(tokens)?;
    expect(tokens, Token::OpenParen)?;
    expect(tokens, Token::Void)?;
    expect(tokens, Token::CloseParen)?;
    expect(tokens, Token::OpenBrace)?;
    let s = parse_statement(tokens)?;
    expect(tokens, Token::CloseBrace)?;

    Ok(ast::Function::Function(id, s))
}

fn parse_statement(tokens: &mut Vec<Token>) -> Result<ast::Statement, ParseError> {
    expect(tokens, Token::Return)?;

    let e = parse_exp(tokens, 0)?;

    expect(tokens, Token::Semicolon)?;

    Ok(ast::Statement::Return(e))
}

fn parse_factor(tokens: &mut Vec<Token>) -> Result<ast::Expression, ParseError> {
    let next: &Token = peek(tokens);

    match *next {
        Token::Constant(n) => {
            consume(tokens);
            Ok(ast::Expression::Constant(n))
        }
        Token::BitwiseComplementOperator => {
            consume(tokens);
            let exp = parse_factor(tokens)?;

            Ok(ast::Expression::Unary(
                ast::UnaryOperator::Complement,
                Box::new(exp),
            ))
        }
        Token::NegationOperator => {
            consume(tokens);
            let exp = parse_factor(tokens)?;

            Ok(ast::Expression::Unary(
                ast::UnaryOperator::Negate,
                Box::new(exp),
            ))
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

fn parse_exp(tokens: &mut Vec<Token>, min_prededence: i32) -> Result<ast::Expression, ParseError> {
    let mut left = parse_factor(tokens)?;

    loop {
        let next = peek(tokens);

        if precedence(next) < min_prededence {
            break;
        }

        match next {
            Token::NegationOperator
            | Token::AdditionOperator
            | Token::DivisionOperator
            | Token::MultiplicationOperator
            | Token::RemainderOperator => {
                let op = consume(tokens);
                let right = parse_exp(tokens, precedence(&op) + 1)?;
                left = parse_binop(op, left, right)?;
            }
            _ => break,
        }
    }

    Ok(left)
}

fn parse_binop(
    op: Token,
    left: Expression,
    right: Expression,
) -> Result<ast::Expression, ParseError> {
    let op = match op {
        Token::NegationOperator => Ok(ast::BinaryOperator::Subtract),
        Token::AdditionOperator => Ok(ast::BinaryOperator::Add),
        Token::DivisionOperator => Ok(ast::BinaryOperator::Divide),
        Token::MultiplicationOperator => Ok(ast::BinaryOperator::Multiply),
        Token::RemainderOperator => Ok(ast::BinaryOperator::Remainder),
        _ => Err(ParseError(format!("not binop"))),
    }?;

    Ok(Expression::Binary(op, Box::new(left), Box::new(right)))
}

fn precedence(token: &Token) -> i32 {
    match token {
        Token::MultiplicationOperator | Token::DivisionOperator | Token::RemainderOperator => 50,
        Token::AdditionOperator | Token::NegationOperator => 45,
        _ => 0,
    }
}

fn parse_identifier(tokens: &mut Vec<Token>) -> Result<ast::Identifier, ParseError> {
    let t = expect_fn(tokens, |t| matches!(t, Token::Identifier(_)))?;

    if let Token::Identifier(s) = t {
        return Ok(ast::Identifier { s: s });
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
    use crate::ast::Identifier;
    use crate::parse::parse;
    use crate::{ast, token};

    #[test]
    fn valid_parse() {
        let mut result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        assert_eq!(
            result,
            ast::Program::Program(ast::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                ast::Statement::Return(ast::Expression::Constant(1))
            ))
        )
    }

    #[test]
    fn valid_parse_unary() {
        let mut result = token::tokenize(" int main(void) { return ~(-1); } ".into()).unwrap();
        let result = parse(&mut result).unwrap();
        assert_eq!(
            result,
            ast::Program::Program(ast::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                ast::Statement::Return(ast::Expression::Unary(
                    ast::UnaryOperator::Complement,
                    Box::new(ast::Expression::Unary(
                        ast::UnaryOperator::Negate,
                        Box::new(ast::Expression::Constant(1))
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
            ast::Program::Program(ast::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                ast::Statement::Return(ast::Expression::Binary(
                    ast::BinaryOperator::Add,
                    Box::new(ast::Expression::Constant(1)),
                    Box::new(ast::Expression::Constant(2)),
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
            ast::Program::Program(ast::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                ast::Statement::Return(ast::Expression::Binary(
                    ast::BinaryOperator::Add,
                    Box::new(ast::Expression::Binary(
                        ast::BinaryOperator::Add,
                        Box::new(ast::Expression::Constant(1)),
                        Box::new(ast::Expression::Constant(2)),
                    )),
                    Box::new(ast::Expression::Constant(3)),
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
            ast::Program::Program(ast::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                ast::Statement::Return(ast::Expression::Binary(
                    ast::BinaryOperator::Add,
                    Box::new(ast::Expression::Constant(1)),
                    Box::new(ast::Expression::Binary(
                        ast::BinaryOperator::Add,
                        Box::new(ast::Expression::Constant(2)),
                        Box::new(ast::Expression::Constant(3)),
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
            ast::Program::Program(ast::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                ast::Statement::Return(ast::Expression::Binary(
                    ast::BinaryOperator::Add,
                    Box::new(ast::Expression::Constant(1)),
                    Box::new(ast::Expression::Binary(
                        ast::BinaryOperator::Multiply,
                        Box::new(ast::Expression::Constant(2)),
                        Box::new(ast::Expression::Constant(3)),
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
            ast::Program::Program(ast::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                ast::Statement::Return(ast::Expression::Binary(
                    ast::BinaryOperator::Multiply,
                    Box::new(ast::Expression::Binary(
                        ast::BinaryOperator::Add,
                        Box::new(ast::Expression::Constant(1)),
                        Box::new(ast::Expression::Constant(2)),
                    )),
                    Box::new(ast::Expression::Constant(3)),
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
            ast::Program::Program(ast::Function::Function(
                Identifier {
                    s: "main".to_string()
                },
                ast::Statement::Return(ast::Expression::Binary(
                    ast::BinaryOperator::Multiply,
                    Box::new(ast::Expression::Binary(
                        ast::BinaryOperator::Add,
                        Box::new(ast::Expression::Unary(
                            ast::UnaryOperator::Negate,
                            Box::new(ast::Expression::Constant(1))
                        )),
                        Box::new(ast::Expression::Constant(2)),
                    )),
                    Box::new(ast::Expression::Unary(
                        ast::UnaryOperator::Complement,
                        Box::new(ast::Expression::Constant(3))
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
