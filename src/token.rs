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
        let c = caps.get(0).unwrap().as_str();

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
        let result = token::tokenize(" int main(void) { return 1; } ".into()).unwrap();
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
