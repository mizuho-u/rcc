use regex::Regex;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Identifier(String),
    Constant(i32),
    Int,
    Void,
    Return,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    BitwiseComplementOperator,
    NegationOperator,
    DecrementOperator,
    AdditionOperator,
    MultiplicationOperator,
    DivisionOperator,
    RemainderOperator,
    AndOperator,
    OrOperator,
    XorOperator,
    LeftShiftOperator,
    RightShiftOperator,
    LogicalNotOperator,
    LogicalAndOperator,
    LogicalOrOperator,
    EqualToOperator,
    NotEqualToOperator,
    LessThanOperator,
    GreaterThanOperator,
    LessOrEqualOperator,
    GreaterOrEqualOperator,
    AssignmentOperator,
    // @todo typedef(GLR parser)
}

pub fn tokenize(p: Vec<u8>) -> Result<Vec<Token>, &'static str> {
    let s = String::from_utf8_lossy(&p);

    match _tokenize(&s, &mut Vec::new()) {
        Ok(ts) => Ok(ts.clone()),
        Err(err) => Err(err),
    }
}

fn _tokenize<'a>(s: &str, ts: &'a mut Vec<Token>) -> Result<&'a Vec<Token>, &'static str> {
    if s.len() <= 0 {
        return Ok(ts);
    }

    let s = s.trim();

    if let Some(t) = find_token(s, r"^(--|<<|>>|&&|\|\||==|!=|<=|>=)", |c| match c {
        "--" => Some(Token::DecrementOperator),
        "<<" => Some(Token::LeftShiftOperator),
        ">>" => Some(Token::RightShiftOperator),
        "&&" => Some(Token::LogicalAndOperator),
        "||" => Some(Token::LogicalOrOperator),
        "==" => Some(Token::EqualToOperator),
        "!=" => Some(Token::NotEqualToOperator),
        "<=" => Some(Token::LessOrEqualOperator),
        ">=" => Some(Token::GreaterOrEqualOperator),
        _ => None,
    }) {
        ts.push(t.0);
        return _tokenize(&s[t.1..], ts);
    }

    if let Some(t) = find_token(s, r"^(~|-|\+|\*|/|%|&|\||\^|!|<|>|=)", |c| match c {
        "~" => Some(Token::BitwiseComplementOperator),
        "-" => Some(Token::NegationOperator),
        "+" => Some(Token::AdditionOperator),
        "*" => Some(Token::MultiplicationOperator),
        "/" => Some(Token::DivisionOperator),
        "%" => Some(Token::RemainderOperator),
        "&" => Some(Token::AndOperator),
        "|" => Some(Token::OrOperator),
        "^" => Some(Token::XorOperator),
        "!" => Some(Token::LogicalNotOperator),
        "<" => Some(Token::LessThanOperator),
        ">" => Some(Token::GreaterThanOperator),
        "=" => Some(Token::AssignmentOperator),
        _ => None,
    }) {
        ts.push(t.0);
        return _tokenize(&s[t.1..], ts);
    }

    if let Some(t) = find_token(s, r"^[a-zA-Z_]\w*\b", |c| match c {
        "int" => Some(Token::Int),
        "void" => Some(Token::Void),
        "return" => Some(Token::Return),
        _ => Some(Token::Identifier(c.to_string().clone())),
    }) {
        ts.push(t.0);
        return _tokenize(&s[t.1..], ts);
    };

    if let Some(t) = find_token(s, r"^[0-9]+\b", |c| {
        let i: i32 = c.parse().unwrap();
        Some(Token::Constant(i))
    }) {
        ts.push(t.0);
        return _tokenize(&s[t.1..], ts);
    };

    if let Some(t) = find_token(s, r"^[(){};]", |c| match c {
        "(" => Some(Token::OpenParen),
        ")" => Some(Token::CloseParen),
        "{" => Some(Token::OpenBrace),
        "}" => Some(Token::CloseBrace),
        ";" => Some(Token::Semicolon),
        _ => None,
    }) {
        ts.push(t.0);
        return _tokenize(&s[t.1..], ts);
    };

    return Err("invalid token");
}

fn find_token(
    s: &str,
    pattern: &str,
    to_token: fn(&str) -> Option<Token>,
) -> Option<(Token, usize)> {
    let re = Regex::new(pattern).unwrap();
    if let Some(caps) = re.captures(s) {
        let c: &str = caps.get(0).unwrap().as_str();

        if let Some(t) = to_token(c) {
            return Some((t, c.len()));
        }
    }

    None
}
