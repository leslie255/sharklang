#[derive(Debug)]
pub enum TokenClass {
    RoundParen,  // ()
    RectParen, // []
    BigParen,    // {}
    Number,
    Name,
}

pub struct Token {
    pub class: TokenClass,
    pub value: String,
}

pub fn parse_tokens(source: String) -> Vec<Token> {
    let mut i = 0;
    let mut tokens: Vec<Token> = Vec::new();
    while i < source.len() {
        let mut ch = source.chars().nth(i).unwrap();
        if ch.is_whitespace() {
            i += 1;
            continue;
        }
        if ch == '(' || ch == ')' {
            tokens.push(Token {
                class: TokenClass::RoundParen,
                value: String::from(ch),
            });
            i += 1;
            continue;
        }
        if ch == '[' || ch == ']' {
            tokens.push(Token {
                class: TokenClass::RectParen,
                value: String::from(ch),
            });
            i += 1;
            continue;
        }
        if ch == '{' || ch == '}' {
            tokens.push(Token {
                class: TokenClass::BigParen,
                value: String::from(ch),
            });
            i += 1;
            continue;
        }
        if ch.is_numeric() {
            let mut content: String = String::new();
            while i < source.len() {
                ch = source.chars().nth(i).unwrap();
                if !ch.is_numeric() {
                    break;
                }
                content.push(ch);
                i += 1;
            }
            tokens.push(Token {
                class: TokenClass::Number,
                value: content,
            });
            continue;
        }
        if ch.is_alphabetic() {
            let mut content: String = String::new();
            while i < source.len() {
                ch = source.chars().nth(i).unwrap();
                if !ch.is_alphanumeric() {
                    break;
                }
                content.push(ch);
                i += 1;
            }
            tokens.push(Token {
                class: TokenClass::Name,
                value: content,
            });
            continue;
        }

    }
    tokens
}

