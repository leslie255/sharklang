use std::rc::Rc;

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
    Underscore,
    Number(Rc<String>),
    String(String),
    Char(u8),
    Identifier(Rc<String>),
    Let,
    Loop,
    If,
    Else,
    Func,
    Return,
    ReturnArrow,
    Break,
    Continue,
    Squiggle,
    And,
    Dollar,
    Star,

    SingleLineCommentStart,
    NewLine,

    RawASM(Rc<String>),

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

    pub fn as_identifier(&self) -> Option<&Rc<String>> {
        if let Self::Identifier(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the token content is [`String`].
    ///
    /// [`String`]: TokenContent::String
    #[must_use]
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(..))
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

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    pub tokens: &'a Vec<Token>,
    pub i: usize,

    has_started: bool,
    eof: Token,
}

impl<'a> TokenStream<'a> {
    pub fn from(tokens: &Vec<Token>) -> TokenStream {
        TokenStream {
            tokens,
            i: 0,
            has_started: false,
            eof: Token::eof(),
        }
    }
    pub fn next(&mut self) -> &Token {
        match self.tokens.get(if self.has_started {
            self.i += 1;
            self.i
        } else {
            self.has_started = true;
            0
        }) {
            Some(token) => token,
            None => &self.eof,
        }
    }
    pub fn peek(&self, i: usize) -> Token {
        match self
            .tokens
            .get(if self.has_started { self.i + i } else { 0 })
        {
            Some(token) => token.clone(),
            None => Token::eof(),
        }
    }
    pub fn current(&self) -> &Token {
        match self.tokens.get(if self.has_started { self.i } else { 0 }) {
            Some(token) => token,
            None => &self.eof,
        }
    }
}

trait StringCustomFuncs {
    fn is_asm_instruction(&self) -> bool;
    fn is_alphanumeric_or_underscore(&self) -> bool;
}
impl StringCustomFuncs for String {
    fn is_asm_instruction(&self) -> bool {
        match self.as_str() {
            "mov" | "push" | "pop" | "ret" | "call" | "cmp" | "add" | "sub" | "mul" | "div"
            | "iadd" | "isub" | "imul" | "idiv" | "extern" | "inc" | "dec" | "jmp" | "jo"
            | "jno" | "js" | "jns" | "je" | "jz" | "jne" | "jnz" | "jp" | "jpe" | "jnp" | "jpo"
            | "jcxz" | "jecxz" | "jb" | "jnae" | "jc" | "jnb" | "jae" | "jnc" | "jbe" | "jna"
            | "ja" | "jnbe" | "jl" | "jnge" | "jge" | "jnl" | "jle" | "jng" | "jg" | "jnle"
            | "db" | "dw" | "dq" | "xor" => true,
            _ => false,
        }
    }

    fn is_alphanumeric_or_underscore(&self) -> bool {
        for ch in self.chars() {
            if !ch.is_alphanumeric_or_underscore() {
                return false;
            }
        }
        true
    }
}

#[derive(Debug, Default, Clone)]
pub struct Token {
    pub content: TokenContent,
    pub position: usize,
    pub len: usize,
}

impl Token {
    fn new(content: TokenContent, position: usize, len: usize) -> Token {
        Token {
            content,
            position,
            len,
        }
    }
    fn eof() -> Token {
        Token {
            content: TokenContent::EOF,
            position: 0,
            len: 0,
        }
    }
    pub fn indicates_end_of_expr(&self) -> bool {
        match self.content {
            TokenContent::EOF
            | TokenContent::Comma
            | TokenContent::BigParenOpen
            | TokenContent::BigParenClose
            | TokenContent::RoundParenClose
            | TokenContent::RectParenClose
            | TokenContent::Semicolon => true,
            _ => false,
        }
    }
    fn from_word(word: &String, i: usize) -> Option<Token> {
        macro_rules! return_token {
            ($content: expr, $len: expr) => {{
                return Some(Token {
                    content: $content,
                    position: i,
                    len: $len,
                });
            }};
        }
        match word.as_str() {
            "(" => return_token!(TokenContent::RoundParenOpen, 1),
            ")" => return_token!(TokenContent::RoundParenClose, 1),
            "[" => return_token!(TokenContent::RectParenOpen, 1),
            "]" => return_token!(TokenContent::RectParenClose, 1),
            "{" => return_token!(TokenContent::BigParenOpen, 1),
            "}" => return_token!(TokenContent::BigParenClose, 1),
            "=" => return_token!(TokenContent::Equal, 1),
            "true" => return_token!(TokenContent::True, 1),
            "false" => return_token!(TokenContent::False, 1),
            ";" => return_token!(TokenContent::Semicolon, 1),
            "," => return_token!(TokenContent::Comma, 1),
            "." => return_token!(TokenContent::Period, 1),
            ":" | "_" => return_token!(TokenContent::Colon, 1),
            "let" => return_token!(TokenContent::Let, 1),
            "loop" => return_token!(TokenContent::Loop, 1),
            "if" => return_token!(TokenContent::If, 1),
            "else" => return_token!(TokenContent::Else, 1),
            "func" => return_token!(TokenContent::Func, 1),
            "return" => return_token!(TokenContent::Return, 1),
            "->" => return_token!(TokenContent::ReturnArrow, 1),
            "break" => return_token!(TokenContent::Break, 1),
            "continue" => return_token!(TokenContent::Continue, 1),
            "~" => return_token!(TokenContent::Squiggle, 1),
            "&" => return_token!(TokenContent::And, 1),
            "$" => return_token!(TokenContent::Dollar, 1),
            "*" => return_token!(TokenContent::Star, 1),
            "//" => return_token!(TokenContent::SingleLineCommentStart, 1),
            "\n" => return_token!(TokenContent::NewLine, 1),
            _ => {}
        }
        let first_ch = word.chars().next()?;
        if first_ch.is_alphabetic() || first_ch == '_' {
            let len = word.len();
            Some(Token {
                content: TokenContent::Identifier(Rc::new(word.clone())),
                position: i,
                len,
            })
        } else if first_ch.is_numeric() {
            let len = word.len();
            return_token!(TokenContent::Number(Rc::new(word.clone())), len);
        } else if first_ch == '\"' {
            let len = word.len();
            let str_content = String::from(&word[1..len]);
            return_token!(TokenContent::String(str_content), len);
        } else {
            None
        }
    }
}

fn should_keep_looking(word: &String) -> bool {
    if word.is_empty() {
        return true;
    }
    let mut chars = word.chars();
    let first_ch = if let Some(c) = chars.next() {
        c
    } else {
        return false;
    };
    // is string
    if first_ch == '\"' {
        if word.len() == 1 {
            return true;
        }
        let c = chars.next_back();
        if c == Some('\"') {
            if chars.next_back() != Some('\\') {
                return false;
            }
        }
        return true;
    }
    // is keyword
    match word.as_str() {
        "(" | ")" | "[" | "]" | "{" | "}" | "=" | "true" | "false" | ";" | "," | "." | ":"
        | "_" | "let" | "loop" | "if" | "else" | "func" | "return" | "-" | "->" | "break"
        | "continue" | "~" | "&" | "$" | "*" | "//" | "\n" => return true,
        _ => (),
    }
    // is a number
    if first_ch.is_numeric() {
        let mut has_dot = false;
        for ch in word.chars() {
            if ch == '.' {
                if has_dot {
                    return false;
                }
                has_dot = true;
            } else if !ch.is_alphanumeric() {
                return false;
            }
        }
        return true;
    }
    // is a symbol
    if word.is_alphanumeric_or_underscore() {
        return true;
    }
    false
}

pub fn parse_into_tokens(source: &String) -> Vec<Token> {
    let mut tokens = Vec::<Token>::new();

    let mut word_start = 0usize;
    let mut word = String::new();
    for (i, ch) in source.char_indices() {
        if !should_keep_looking(&word) {
            let last_ch = word.pop();
            if word.len() == 0 {
                word.push(ch);
                continue;
            }
            if let Some(t) = Token::from_word(&word, word_start) {
                tokens.push(t);
            }
            word.clear();
            if let Some(last_ch) = last_ch {
                if !last_ch.is_whitespace()
                    && if let Some(t) = tokens.last() {
                        !t.content.is_string()
                    } else {
                        false
                    }
                {
                    word.push(last_ch);
                    word_start = i - 1;
                } else {
                    word_start = i;
                }
            }
        } else {
        }
        word.push(ch);
    }
    word = word.trim_end().to_string();
    if !word.is_empty() {
        if let Some(t) = Token::from_word(&word, word_start) {
            tokens.push(t);
        }
    }

    tokens
}
