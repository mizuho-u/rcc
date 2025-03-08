use crate::token::{self, Token};

#[derive(PartialEq, Debug)]
pub enum Program {
    Program(Vec<FunctionDeclaration>),
}

#[derive(PartialEq, Debug)]
pub enum Function {
    Function(Identifier, Block),
}

#[derive(PartialEq, Debug)]
pub enum Block {
    Block(Vec<BlockItem>),
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
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Null,
    Goto(Identifier),
    Label(Identifier, Box<Statement>),
    Compound(Block),
    Break(Identifier),
    Continue(Identifier),
    While(Expression, Box<Statement>, Identifier),
    DoWhile(Box<Statement>, Expression, Identifier),
    For(
        ForInit,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
        Identifier,
    ),
    Switch(
        Expression,
        Box<Statement>,
        Vec<(Option<Expression>, Identifier)>,
        Identifier,
    ),
    Case(Expression, Option<Box<Statement>>, Identifier),
    Default(Option<Box<Statement>>, Identifier),
}

#[derive(PartialEq, Debug)]
pub enum ForInit {
    Declaration(Declaration),
    Expression(Option<Expression>),
}

#[derive(PartialEq, Debug)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

#[derive(PartialEq, Debug)]
pub struct VariableDeclaration(pub Identifier, pub Option<Expression>);

#[derive(PartialEq, Debug)]
pub struct FunctionDeclaration(pub Identifier, pub Vec<Identifier>, pub Option<Block>);

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Var(Identifier),
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(AssignmentOperator, Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum AssignmentOperator {
    Simple,
    Addition,
    Subtract,
    Multiplication,
    Division,
    Remainder,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
    IncrementPrefix,
    IncrementPostfix,
    DecrementPrefix,
    DecrementPostfix,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOperator {
    Subtract,
    Add,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    Xor,
    LeftShift,
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

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn placeholder() -> Identifier {
        Identifier("ph".to_string())
    }
}

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
    let mut fs = Vec::new();
    loop {
        if tokens.len() == 0 {
            break;
        }

        fs.push(parse_function_declaration(tokens)?);
    }

    Ok(Program::Program(fs))
}

fn parse_function_declaration(tokens: &mut Vec<Token>) -> Result<FunctionDeclaration, ParseError> {
    expect(tokens, Token::Int)?;
    let id = parse_identifier(tokens)?;
    let params = parse_func_params(tokens)?;

    let next = peek(tokens).expect("peek failed, no token left.");
    let block = if *next == Token::Semicolon {
        consume(tokens);
        None
    } else {
        Some(parse_block(tokens)?)
    };

    Ok(FunctionDeclaration(id, params, block))
}

fn parse_func_params(tokens: &mut Vec<Token>) -> Result<Vec<Identifier>, ParseError> {
    expect(tokens, Token::OpenParen)?;

    let mut ids = Vec::new();

    let next = peek(tokens).expect("peek failed, no token left.");

    match next {
        Token::Void => {
            consume(tokens);
        }
        Token::CloseParen => {}
        _ => loop {
            expect(tokens, Token::Int)?;
            ids.push(parse_identifier(tokens)?);

            let next = peek(tokens).expect("peek failed, no token left.");
            if *next == Token::CloseParen {
                break;
            }

            expect(tokens, Token::Comma)?;
        },
    }

    expect(tokens, Token::CloseParen)?;

    Ok(ids)
}

fn parse_block(tokens: &mut Vec<Token>) -> Result<Block, ParseError> {
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

    Ok(Block::Block(body))
}

fn parse_block_item(tokens: &mut Vec<Token>) -> Result<BlockItem, ParseError> {
    let next = peek(tokens).expect("peek failed, no token left.");

    match next {
        Token::Int => Ok(BlockItem::Declaration(parse_declaration(tokens)?)),
        _ => Ok(BlockItem::Statement(parse_statement(tokens)?)),
    }
}

fn parse_declaration(tokens: &mut Vec<Token>) -> Result<Declaration, ParseError> {
    let _type = peek_ahead(tokens, 0)?;
    let _id = peek_ahead(tokens, 1)?;

    if *peek_ahead(tokens, 2)? == Token::OpenParen {
        Ok(Declaration::Function(parse_function_declaration(tokens)?))
    } else {
        Ok(Declaration::Variable(parse_variable_declaration(tokens)?))
    }
}

fn parse_variable_declaration(tokens: &mut Vec<Token>) -> Result<VariableDeclaration, ParseError> {
    let _type = expect(tokens, Token::Int)?;
    let id = parse_identifier(tokens)?;

    let next = peek(tokens).expect("peek failed, no token left.");

    if *next == Token::Semicolon {
        consume(tokens);
        return Ok(VariableDeclaration(id, None));
    }

    expect(tokens, Token::AssignmentOperator)?;
    let exp = parse_exp(tokens, 0)?;
    expect(tokens, Token::Semicolon)?;

    Ok(VariableDeclaration(id, Some(exp)))
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
        Token::If => {
            consume(tokens);
            expect(tokens, Token::OpenParen)?;
            let cond = parse_exp(tokens, 0)?;
            expect(tokens, Token::CloseParen)?;
            let then = parse_statement(tokens)?;

            let next = peek(tokens).expect("peek failed, no token left.");
            if *next == Token::Else {
                consume(tokens);
                let el = parse_statement(tokens)?;

                Ok(Statement::If(cond, Box::new(then), Some(Box::new(el))))
            } else {
                Ok(Statement::If(cond, Box::new(then), None))
            }
        }
        Token::Goto => {
            consume(tokens);
            if let Token::Identifier(i) = consume(tokens) {
                consume(tokens);
                Ok(Statement::Goto(Identifier(i)))
            } else {
                Err(ParseError(format!("expect identifier")))
            }
        }
        Token::Identifier(_) => {
            if let Some(id) = peek_label(tokens) {
                consume(tokens);
                consume(tokens);

                Ok(Statement::Label(id, Box::new(parse_statement(tokens)?)))
            } else {
                Ok(parse_statement_expression(tokens)?)
            }
        }
        Token::OpenBrace => Ok(Statement::Compound(parse_block(tokens)?)),
        Token::Break => {
            consume(tokens);
            expect(tokens, Token::Semicolon)?;
            Ok(Statement::Break(Identifier::placeholder()))
        }
        Token::Continue => {
            consume(tokens);
            expect(tokens, Token::Semicolon)?;
            Ok(Statement::Continue(Identifier::placeholder()))
        }
        Token::While => {
            consume(tokens);

            expect(tokens, Token::OpenParen)?;
            let cond = parse_exp(tokens, 0)?;
            expect(tokens, Token::CloseParen)?;

            Ok(Statement::While(
                cond,
                Box::new(parse_statement(tokens)?),
                Identifier::placeholder(),
            ))
        }
        Token::Do => {
            consume(tokens);

            let body = parse_statement(tokens)?;
            expect(tokens, Token::While)?;

            expect(tokens, Token::OpenParen)?;
            let cond = parse_exp(tokens, 0)?;
            expect(tokens, Token::CloseParen)?;

            expect(tokens, Token::Semicolon)?;

            Ok(Statement::DoWhile(
                Box::new(body),
                cond,
                Identifier::placeholder(),
            ))
        }
        Token::For => {
            consume(tokens);
            expect(tokens, Token::OpenParen)?;

            let next = peek(tokens).expect("peek failed, no token left.");
            let init = if *next == Token::Int {
                ForInit::Declaration(Declaration::Variable(parse_variable_declaration(tokens)?))
            } else if *next == Token::Semicolon {
                consume(tokens);
                ForInit::Expression(None)
            } else {
                let e = parse_exp(tokens, 0)?;
                expect(tokens, Token::Semicolon)?;
                ForInit::Expression(Some(e))
            };

            let next = peek(tokens).expect("peek failed, no token left.");
            let cond = if *next == Token::Semicolon {
                None
            } else {
                Some(parse_exp(tokens, 0)?)
            };
            expect(tokens, Token::Semicolon)?;

            let next = peek(tokens).expect("peek failed, no token left.");
            let post = if *next == Token::CloseParen {
                None
            } else {
                Some(parse_exp(tokens, 0)?)
            };
            expect(tokens, Token::CloseParen)?;

            Ok(Statement::For(
                init,
                cond,
                post,
                Box::new(parse_statement(tokens)?),
                Identifier::placeholder(),
            ))
        }
        Token::Switch => {
            consume(tokens);
            expect(tokens, Token::OpenParen)?;
            let ctrl = parse_exp(tokens, 0)?;
            expect(tokens, Token::CloseParen)?;

            let body = parse_statement(tokens)?;

            Ok(Statement::Switch(
                ctrl,
                Box::new(body),
                vec![],
                Identifier::placeholder(),
            ))
        }
        Token::Case => {
            consume(tokens);
            let exp = parse_exp(tokens, 0)?;
            expect(tokens, Token::Colon)?;

            let next = peek(tokens).expect("peek failed, no token left.");

            let body = if *next == Token::CloseBrace {
                None
            } else {
                Some(Box::new(parse_statement(tokens)?))
            };

            Ok(Statement::Case(exp, body, Identifier::placeholder()))
        }
        Token::Default => {
            consume(tokens);
            expect(tokens, Token::Colon)?;

            let next = peek(tokens).expect("peek failed, no token left.");

            let body = if *next == Token::CloseBrace {
                None
            } else {
                Some(Box::new(parse_statement(tokens)?))
            };

            Ok(Statement::Default(body, Identifier::placeholder()))
        }
        _ => Ok(parse_statement_expression(tokens)?),
    }
}

fn peek_label(tokens: &Vec<Token>) -> Option<Identifier> {
    let next = peek(tokens).expect("peek failed, no token left.");
    if let Token::Identifier(id) = next {
        let next_two_ahead = peek_two_ahead(tokens).expect("peek failed, no token left.");
        if *next_two_ahead == Token::Colon {
            return Some(Identifier(id.clone()));
        }
    }

    return None;
}

fn parse_statement_expression(tokens: &mut Vec<Token>) -> Result<Statement, ParseError> {
    let e = parse_exp(tokens, 0)?;
    expect(tokens, Token::Semicolon)?;
    Ok(Statement::Expression(e))
}

fn parse_factor(tokens: &mut Vec<Token>) -> Result<Expression, ParseError> {
    let token = consume(tokens);

    let mut e = match token {
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
            let exp = parse_factor(tokens)?;
            Expression::Unary(UnaryOperator::IncrementPrefix, Box::new(exp))
        }
        Token::DecrementOperator => {
            let exp = parse_factor(tokens)?;
            Expression::Unary(UnaryOperator::DecrementPrefix, Box::new(exp))
        }
        _ => return Err(ParseError(format!("Malformed expression {:?}", token))),
    };

    // functions
    let next = peek(tokens).expect("peek failed, no token left.");
    if *next == Token::OpenParen {
        consume(tokens);
        match e {
            Expression::Var(ref id) => {
                let mut args = Vec::new();

                let next = peek(tokens).expect("peek failed, no token left.");
                if *next != Token::CloseParen {
                    loop {
                        args.push(parse_exp(tokens, 0)?);

                        let next = peek(tokens).expect("peek failed, no token left.");
                        if *next == Token::CloseParen {
                            break;
                        }

                        expect(tokens, Token::Comma)?;
                    }
                }

                expect(tokens, Token::CloseParen)?;
                e = Expression::FunctionCall(id.clone(), args);
            }
            Expression::Unary(_, _) => {}
            _ => return Err(ParseError("invalid function call".to_string())),
        }
    }

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

        if let Some(aop) = peek_assignment_operator(tokens) {
            let op = consume(tokens);

            let right = parse_exp(tokens, precedence(&op))?;

            left = Expression::Assignment(aop, Box::new(left), Box::new(right));
        } else if *next == Token::QuestionMark {
            let op = consume(tokens);

            let middle = parse_exp(tokens, 0)?;
            expect(tokens, Token::Colon)?;

            let right = parse_exp(tokens, precedence(&op))?;

            left = Expression::Conditional(Box::new(left), Box::new(middle), Box::new(right));
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

fn peek_assignment_operator(tokens: &Vec<Token>) -> Option<AssignmentOperator> {
    let op = peek(tokens).expect("peek failed, no token left.");

    let aop = match *op {
        Token::AssignmentOperator => AssignmentOperator::Simple,
        Token::CompoundAssignmentAdditionOperator => AssignmentOperator::Addition,
        Token::CompoundAssignmentSubtractOperator => AssignmentOperator::Subtract,
        Token::CompoundAssignmentMultiplicationOperator => AssignmentOperator::Multiplication,
        Token::CompoundAssignmentDivisionOperator => AssignmentOperator::Division,
        Token::CompoundAssignmentRemainderOperator => AssignmentOperator::Remainder,
        Token::CompoundAssignmentAndOperator => AssignmentOperator::And,
        Token::CompoundAssignmentOrOperator => AssignmentOperator::Or,
        Token::CompoundAssignmentXorOperator => AssignmentOperator::Xor,
        Token::CompoundAssignmentLeftShiftOperator => AssignmentOperator::LeftShift,
        Token::CompoundAssignmentRightShiftOperator => AssignmentOperator::RightShift,
        _ => return None,
    };

    Some(aop)
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
        Token::LeftShiftOperator => Ok(BinaryOperator::LeftShift),
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
        Token::QuestionMark => 5,
        Token::AssignmentOperator
        | Token::CompoundAssignmentAdditionOperator
        | Token::CompoundAssignmentSubtractOperator
        | Token::CompoundAssignmentMultiplicationOperator
        | Token::CompoundAssignmentDivisionOperator
        | Token::CompoundAssignmentRemainderOperator
        | Token::CompoundAssignmentAndOperator
        | Token::CompoundAssignmentOrOperator
        | Token::CompoundAssignmentXorOperator
        | Token::CompoundAssignmentLeftShiftOperator
        | Token::CompoundAssignmentRightShiftOperator => 1,
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

fn peek_two_ahead(tokens: &Vec<Token>) -> Option<&Token> {
    if tokens.len() == 0 {
        None
    } else {
        Some(&tokens[1])
    }
}

fn peek_ahead(tokens: &Vec<Token>, ahead: usize) -> Result<&Token, ParseError> {
    if tokens.len() == 0 {
        Err(ParseError("peek failed, no token left.".to_string()))
    } else {
        Ok(&tokens[ahead])
    }
}

fn consume(tokens: &mut Vec<Token>) -> Token {
    tokens.remove(0)
}
