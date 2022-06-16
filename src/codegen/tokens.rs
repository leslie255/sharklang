#[derive(Debug, PartialEq, Clone)]
#[allow(unused)]
pub enum TokenClass {
    RoundParenOpen,
    RoundParenClose,
    RectParenOpen,
    RectParenClose,
    BigParenOpen,
    BigParenClose,
    Equal,
    True,
    False,
    Semicolon,
    Period,
    UInt(u64),
    Float(f64),
    String(String),
    Identifier(String),
    Let,
    Set,

    Unknown,
}
impl Default for TokenClass {
    fn default() -> Self {
        return TokenClass::Unknown;
    }
}

trait CharCustomFuncs {
    fn is_alphanumeric_or_underscore(&self) -> bool;
    fn is_paren(&self) -> bool;
}
impl CharCustomFuncs for char {
    fn is_alphanumeric_or_underscore(&self) -> bool {
        self.is_alphanumeric() || *self == '_'
    }
    fn is_paren(&self) -> bool {
        *self == '(' || *self == ')' || *self == '[' || *self == ']' || *self == '{' || *self == '}'
    }
}

#[derive(Debug, Default, Clone)]
pub struct Token {
    pub class: TokenClass,
    pub line: u64,
    pub column: u64,
}

impl Token {
    fn from(str: &String, line: u64, column: u64) -> Token {
        Token {
            class: match str.as_str() {
                "(" => TokenClass::RoundParenOpen,
                ")" => TokenClass::RoundParenClose,
                "[" => TokenClass::RectParenOpen,
                "]" => TokenClass::RectParenClose,
                "{" => TokenClass::BigParenOpen,
                "}" => TokenClass::BigParenClose,
                "=" => TokenClass::Equal,
                "true" => TokenClass::True,
                "false" => TokenClass::False,
                "let" => TokenClass::Let,
                "set" => TokenClass::Set,
                ";" => TokenClass::Semicolon,
                "." => TokenClass::Period,
                _ => {
                    if str
                        .chars()
                        .nth(0)
                        .expect("trying to parse a token from empty string")
                        .is_numeric()
                    {
                        TokenClass::UInt(
                            str.parse()
                                .unwrap_or_else(|_| panic!("`{}` is not an unsigned integar", str)),
                        )
                    } else {
                        TokenClass::Identifier(str.clone())
                    }
                }
            },
            line: line.clone(),
            column: column.clone(),
        }
    }
}

pub fn parse_tokens(source: String) -> Vec<Token> {
    // TODO: add line & column number for each token
    let mut tokens: Vec<Token> = Vec::new();
    let mut line: u64 = 0;
    let mut column: u64 = 0;
    let mut last_word = String::new();
    let mut iter = source.chars();
    let mut ch: Option<char> = iter.next();
    while ch.is_some() {
        column += 1;
        match ch.unwrap() {
            '\n' => {
                line += 1;
                column = 0;
            }
            '\"' => {
                // string
                ch = iter.next();
                while ch != Some('\"') {
                    last_word.push(ch.unwrap());
                    ch = iter.next();
                    if ch.unwrap() == '\n' {
                        line += 1;
                    }
                }
                tokens.push(Token {
                    class: TokenClass::String(last_word),
                    line: line.clone(),
                    column: column,
                });
                last_word = String::new();
            }
            ' ' => {
                tokens.push(Token::from(&last_word, line, column));
                last_word = String::new();
            }
            _ => {
                if last_word.is_empty() {
                    last_word.push(ch.unwrap());
                    ch = iter.next();
                    continue;
                }
                let last_char = last_word.chars().last().unwrap();
                if ch.unwrap().is_alphanumeric_or_underscore()
                    && last_char.is_alphanumeric_or_underscore()
                {
                    last_word.push(ch.unwrap());
                } else {
                    tokens.push(Token::from(&last_word, line, column));
                    last_word = String::from(ch.unwrap());
                }
            }
        }
        ch = iter.next();
    }
    tokens.push(Token::from(&last_word, line, column));
    tokens
}
