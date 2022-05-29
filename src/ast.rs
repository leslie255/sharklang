#[derive(Debug)]
enum TokenClass {
    RoundParen, // ()
    RectParen,  // []
    BigParen,   // {}
    Equal,      // "=" not "=="
    CompareOP,
    ArithOP,
    SelfArithOP, // +=, -=, ...
    BoolOP,
    Semicolon,
    Comment,
    Number,
    Identifier,
    Unknown,
}

#[derive(Debug)]
struct Token {
    pub class: TokenClass,
    pub value: String,
}

fn _is_paren(ch: char) -> bool {
    ch == '(' || ch == ')' || ch == '{' || ch == '{' || ch == '[' || ch == ']'
}

impl Token {
    fn determine_type(&mut self) {
        if self.value.is_empty() {
            self.class = TokenClass::Unknown;
            return;
        } else if self.value.chars().nth(0).unwrap().is_numeric() {
            self.class = TokenClass::Number;
            return;
        } else if self.value.chars().nth(0).unwrap().is_alphanumeric() {
            self.class = TokenClass::Identifier;
            return;
        }

        self.class = match self.value.as_str() {
            "(" | ")" => TokenClass::RoundParen,
            "[" | "]" => TokenClass::RectParen,
            "{" | "}" => TokenClass::BigParen,
            "=" => TokenClass::Equal,
            "==" | ">" | "<" | ">=" | "<=" | "!=" => TokenClass::CompareOP,
            "+" | "-" | "*" | "/" | "%" => TokenClass::ArithOP,
            "+=" | "-=" | "*=" | "/=" | "%=" => TokenClass::SelfArithOP,
            "!" | "&&" | "||" | "" => TokenClass::BoolOP,
            ";" => TokenClass::Semicolon,
            "//" | "/*" | "*/" => TokenClass::Comment,
            _ => TokenClass::Unknown,
        }
    }
}

fn parse_tokens(source: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![Token {
        class: TokenClass::Unknown,
        value: String::from(""),
    }];
    for i in 0..source.chars().count() - 1 {
        let ch = source.chars().nth(i).unwrap();
        let last_token = &mut tokens.last_mut().unwrap();
        if !ch.is_whitespace() {
            if last_token.value.is_empty() {
                last_token.value.push(ch);
            } else if last_token
                .value
                .chars()
                .nth(last_token.value.len() - 1)
                .unwrap()
                .is_alphanumeric()
                == ch.is_alphanumeric()
            {
                last_token.value.push(ch);
            } else {
                tokens.push(Token {
                    class: TokenClass::Unknown,
                    value: String::from(ch),
                });
            }
        } else if !source
            .chars()
            .nth(i.saturating_sub(1))
            .unwrap_or_else(|| ' ')
            .is_whitespace()
        {
            tokens.push(Token {
                class: TokenClass::Unknown,
                value: String::from(""),
            });
        }
        let last_token = &mut tokens.last_mut().unwrap();
        if last_token.value.is_empty() {
            last_token.value = String::from("");
        } else {
            last_token.determine_type();
        }
    }
    tokens
}

pub fn construct_ast(source: String) {
    // TODO
    let tokens = parse_tokens(source);
    println!("{:#?}", tokens);
}
