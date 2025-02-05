use crate::{
    ast,
    token::{self, Token},
};

#[derive(Debug)]
pub struct ParseError {
    s: String,
}

pub fn parse(tokens: &mut Vec<Token>) -> Result<ast::Program, ParseError> {
    let p = parse_program(tokens)?;

    if tokens.len() >= 1 {
        Err(ParseError {
            s: format!("some tokens remaining {:?}", tokens),
        })
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

    let e = parse_expression(tokens)?;

    expect(tokens, Token::Semicolon)?;

    Ok(ast::Statement::Return(e))
}

fn parse_expression(tokens: &mut Vec<Token>) -> Result<ast::Expression, ParseError> {
    let next: &Token = peek(tokens);

    match *next {
        Token::Constant(n) => {
            consume(tokens);
            Ok(ast::Expression::Constant(n))
        }
        Token::BitwiseComplementOperator => {
            consume(tokens);
            let exp = parse_expression(tokens)?;

            Ok(ast::Expression::Unary(
                ast::UnaryOperator::Complement,
                Box::new(exp),
            ))
        }
        Token::NegationOperator => {
            consume(tokens);
            let exp = parse_expression(tokens)?;

            Ok(ast::Expression::Unary(
                ast::UnaryOperator::Negate,
                Box::new(exp),
            ))
        }
        // Token::DecrementOperator => todo!(),
        Token::OpenParen => {
            consume(tokens);
            let exp = parse_expression(tokens)?;
            expect(tokens, Token::CloseParen)?;
            Ok(exp)
        }
        _ => {
            return Err(ParseError {
                s: format!("Malformed expression {:?}", next),
            })
        }
    }
}

fn parse_identifier(tokens: &mut Vec<Token>) -> Result<ast::Identifier, ParseError> {
    let t = expect_fn(tokens, |t| matches!(t, Token::Identifier(_)))?;

    if let Token::Identifier(s) = t {
        return Ok(ast::Identifier { s: s });
    }

    Err(ParseError {
        s: format!("Invalid Identifier {:?}", t),
    })
}
fn expect(tokens: &mut Vec<Token>, expected: token::Token) -> Result<Token, ParseError> {
    let head = consume(tokens);

    if expected != head {
        return Err(ParseError {
            s: format!("expected {:?} got {:?}", expected, head),
        });
    }

    Ok(head)
}

fn expect_fn(tokens: &mut Vec<Token>, cb: fn(&Token) -> bool) -> Result<Token, ParseError> {
    let head = consume(tokens);

    if !cb(&head) {
        return Err(ParseError {
            s: format!("unexpected token {:?}", head),
        });
    }

    Ok(head)
}

fn peek(tokens: &mut Vec<Token>) -> &Token {
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
    #[should_panic]
    fn invalid_parse() {
        let mut result = token::tokenize(" int (void) { return 1; } ".into()).unwrap();
        parse(&mut result).unwrap();
    }
}
