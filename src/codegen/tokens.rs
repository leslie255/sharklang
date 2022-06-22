#[derive(Debug, PartialEq, Clone)]
pub enum TokenContent {
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
    Comma,
    Colon,
    UInt(u64),
    String(String),
    Identifier(String),
    Let,
    Func,

    RawASM(String),

    Unknown,
    EOF,
}
impl TokenContent {
    pub fn is_eof(&self) -> bool {
        match self {
            Self::EOF => true,
            _ => false,
        }
    }
}
impl Default for TokenContent {
    fn default() -> Self {
        return TokenContent::Unknown;
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

trait StringCustomFuncs {
    fn is_asm_instruction(&self) -> bool;
}
impl StringCustomFuncs for String {
    fn is_asm_instruction(&self) -> bool {
        match self.as_str() {
            "mov" | "push" | "ret" | "call" | "cmp" | "jmp" | "jpe" | "jpz" | "add" | "sub"
            | "mul" | "div" => true,
            _ => false,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Token {
    pub content: TokenContent,
    pub line: u64,
    pub column: u64,
}

impl Token {
    fn from(str: &String, line: u64, column: u64) -> Token {
        Token {
            content: match str.as_str() {
                "(" => TokenContent::RoundParenOpen,
                ")" => TokenContent::RoundParenClose,
                "[" => TokenContent::RectParenOpen,
                "]" => TokenContent::RectParenClose,
                "{" => TokenContent::BigParenOpen,
                "}" => TokenContent::BigParenClose,
                "=" => TokenContent::Equal,
                "true" => TokenContent::True,
                "false" => TokenContent::False,
                "let" => TokenContent::Let,
                ";" => TokenContent::Semicolon,
                "." => TokenContent::Period,
                "," => TokenContent::Comma,
                ":" => TokenContent::Colon,
                "func" => TokenContent::Func,
                _ => {
                    if str
                        .chars()
                        .nth(0)
                        .expect("trying to parse a token from empty string")
                        .is_numeric()
                    {
                        TokenContent::UInt(
                            str.parse()
                                .unwrap_or_else(|_| panic!("`{}` is not an unsigned integar", str)),
                        )
                    } else {
                        TokenContent::Identifier(str.clone())
                    }
                }
            },
            line: line.clone(),
            column: column.clone(),
        }
    }
    fn eof() -> Token {
        Token {
            content: TokenContent::EOF,
            line: 0,
            column: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenStream {
    pub tokens: Vec<Token>,
    pub i: usize,

    has_started: bool,
}

impl TokenStream {
    pub fn next(&mut self) -> Token {
        if self.has_started {
            self.i += 1;
            match self.tokens.get(self.i) {
                Some(token) => token.clone(),
                None => Token::eof(),
            }
        } else {
            self.has_started = true;
            self.tokens.first().unwrap().clone()
        }
    }
    pub fn look_ahead(&self, i: usize) -> Token {
        match self.tokens.get(if self.has_started { self.i + i } else { 0 }) {
            Some(token) => token.clone(),
            None => Token::eof(),
        }
    }

    pub fn from(tokens: Vec<Token>) -> TokenStream {
        let mut stream = TokenStream {
            tokens: Vec::new(),
            i: 0,
            has_started: false,
        };

        stream.tokens = tokens;

        stream
    }
}

pub fn parse_tokens<'a>(source: String) -> TokenStream {
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
                if last_word.is_empty() {
                    ch = iter.next();
                    continue;
                }
                tokens.push(Token::from(&last_word, line, column));
                last_word = String::new();
            }
            ' ' | '\t' => {
                if last_word.is_empty() {
                    ch = iter.next();
                    continue;
                }
                if last_word.is_asm_instruction() {
                    while ch != Some('\n') {
                        last_word.push(ch.unwrap());
                        ch = iter.next();
                    }
                    line += 1;
                    tokens.push(Token {
                        content: TokenContent::RawASM(last_word),
                        line: line.clone(),
                        column: column.clone(),
                    });
                    last_word = String::new();
                } else {
                    tokens.push(Token::from(&last_word, line, column));
                    last_word = String::new();
                }
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
                    content: TokenContent::String(last_word),
                    line: line.clone(),
                    column: column.clone(),
                });
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
    if !last_word.is_empty() {
        tokens.push(Token::from(&last_word, line, column));
    }
    TokenStream::from(tokens)
}
