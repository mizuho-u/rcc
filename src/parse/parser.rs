use crate::token::{self, Token};

#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Function),
}

#[derive(PartialEq, Debug)]
pub enum Function {
    Function(Identifier, Vec<BlockItem>),
}

#[derive(PartialEq, Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Null,
}

#[derive(PartialEq, Debug)]
pub enum Declaration {
    Declaration(Identifier, Option<Expression>),
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Var(Identifier),
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
    IncrementPrefix,
    IncrementPostfix,
    DecrementPrefix,
    DecrementPostfix,
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
    let f = parse_function_definition(tokens)?;
    Ok(Program::Program(f))
}

fn parse_function_definition(tokens: &mut Vec<Token>) -> Result<Function, ParseError> {
    expect(tokens, Token::Int)?;
    let id = parse_identifier(tokens)?;
    expect(tokens, Token::OpenParen)?;
    expect(tokens, Token::Void)?;
    expect(tokens, Token::CloseParen)?;
    expect(tokens, Token::OpenBrace)?;

    let mut body = Vec::new();

    loop {
        let next = peek(tokens).expect("peek failed, no token left.");
        if *next == Token::CloseBrace {
            break;
        }
        body.push(parse_block_item(tokens)?);
    }

    expect(tokens, Token::CloseBrace)?;

    Ok(Function::Function(id, body))
}

fn parse_block_item(tokens: &mut Vec<Token>) -> Result<BlockItem, ParseError> {
    let next = peek(tokens).expect("peek failed, no token left.");

    match next {
        Token::Int => Ok(BlockItem::Declaration(parse_declaration(tokens)?)),
        _ => Ok(BlockItem::Statement(parse_statement(tokens)?)),
    }
}

fn parse_declaration(tokens: &mut Vec<Token>) -> Result<Declaration, ParseError> {
    let _type = expect(tokens, Token::Int)?;
    let id = parse_identifier(tokens)?;

    let next = peek(tokens).expect("peek failed, no token left.");

    if *next == Token::Semicolon {
        consume(tokens);
        return Ok(Declaration::Declaration(id, None));
    }

    expect(tokens, Token::AssignmentOperator)?;
    let exp = parse_exp(tokens, 0)?;
    expect(tokens, Token::Semicolon)?;

    return Ok(Declaration::Declaration(id, Some(exp)));
}

fn parse_statement(tokens: &mut Vec<Token>) -> Result<Statement, ParseError> {
    let next = peek(tokens).expect("peek failed, no token left.");

    match next {
        Token::Semicolon => {
            consume(tokens);
            Ok(Statement::Null)
        }
        Token::Return => {
            consume(tokens);

            let e = parse_exp(tokens, 0)?;

            expect(tokens, Token::Semicolon)?;

            Ok(Statement::Return(e))
        }
        _ => {
            let e = parse_exp(tokens, 0)?;

            expect(tokens, Token::Semicolon)?;

            Ok(Statement::Expression(e))
        }
    }
}

fn parse_factor(tokens: &mut Vec<Token>) -> Result<Expression, ParseError> {
    let next = consume(tokens);

    let mut e = match next {
        Token::Constant(n) => Expression::Constant(n),
        Token::BitwiseComplementOperator => {
            let exp = parse_factor(tokens)?;

            Expression::Unary(UnaryOperator::Complement, Box::new(exp))
        }
        Token::NegationOperator => {
            let exp = parse_factor(tokens)?;

            Expression::Unary(UnaryOperator::Negate, Box::new(exp))
        }
        Token::LogicalNotOperator => {
            let exp: Expression = parse_factor(tokens)?;

            Expression::Unary(UnaryOperator::Not, Box::new(exp))
        }
        Token::OpenParen => {
            let exp = parse_exp(tokens, 0)?;
            expect(tokens, Token::CloseParen)?;
            exp
        }
        Token::Identifier(id) => Expression::Var(Identifier(id)),
        Token::IncrementOperator => {
            let exp = parse_exp(tokens, 0)?;
            Expression::Unary(UnaryOperator::IncrementPrefix, Box::new(exp))
        }
        Token::DecrementOperator => {
            let exp = parse_exp(tokens, 0)?;
            Expression::Unary(UnaryOperator::DecrementPrefix, Box::new(exp))
        }
        _ => return Err(ParseError(format!("Malformed expression {:?}", next))),
    };

    // postfix
    e = loop {
        let next = peek(tokens).expect("peek failed, no token left.");
        match next {
            Token::IncrementOperator => {
                consume(tokens);
                e = Expression::Unary(UnaryOperator::IncrementPostfix, Box::new(e));
            }
            Token::DecrementOperator => {
                consume(tokens);
                e = Expression::Unary(UnaryOperator::DecrementPostfix, Box::new(e));
            }
            _ => break e,
        };
    };

    Ok(e)
}

fn parse_exp(tokens: &mut Vec<Token>, min_prededence: i32) -> Result<Expression, ParseError> {
    let mut left = parse_factor(tokens)?;

    loop {
        let next = peek(tokens).expect("peek failed, no token left.");

        if precedence(next) < min_prededence {
            break;
        }

        if *next == Token::AssignmentOperator {
            let op = consume(tokens);

            let right = parse_exp(tokens, precedence(&op))?;

            left = Expression::Assignment(Box::new(left), Box::new(right));
        } else {
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
                    let right = parse_exp(tokens, precedence(&op) + 1)?;
                    left = parse_binop(op, left, right)?;
                }
                _ => break,
            }
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

fn precedence(token: &Token) -> i32 {
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
        Token::AssignmentOperator => 1,
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

fn peek(tokens: &Vec<Token>) -> Option<&Token> {
    if tokens.len() == 0 {
        None
    } else {
        Some(&tokens[0])
    }
}

fn consume(tokens: &mut Vec<Token>) -> Token {
    tokens.remove(0)
}
