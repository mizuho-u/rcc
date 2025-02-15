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
    LessThanOrEqualOperator,
    GreaterThanOrEqualOperator,
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
        "<=" => Some(Token::LessThanOrEqualOperator),
        ">=" => Some(Token::GreaterThanOrEqualOperator),
        _ => None,
    }) {
        ts.push(t.0);
        return _tokenize(&s[t.1..], ts);
    }

    if let Some(t) = find_token(s, r"^(~|-|\+|\*|/|%|&|\||\^|!|<|>)", |c| match c {
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

#[cfg(test)]
mod tests {
    use crate::token;
    use crate::token::Token;

    #[test]
    fn valid_token() {
        let result = token::tokenize(" int main(void) { return -(~~(--1)); } ".into()).unwrap();
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
                Token::DecrementOperator,
                Token::Constant(1),
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
        let result =
            token::tokenize(" int main(void) { return 1<<(2|3)>>4&5^6; } ".into()).unwrap();
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
                Token::LessThanOrEqualOperator,
                Token::Constant(6),
                Token::GreaterThanOrEqualOperator,
                Token::Constant(7),
                Token::Semicolon,
                Token::CloseBrace,
            ]
        )
    }

    #[test]
    #[should_panic]
    fn invalid_token() {
        token::tokenize(" 1foo ".into()).unwrap();
    }
}
