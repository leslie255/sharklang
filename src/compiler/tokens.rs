use std::{collections::HashMap, fs, path::Path, rc::Rc};

use super::error::CompileError;

#[allow(dead_code)]
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
    Extern,
    Return,
    ReturnArrow,
    Break,
    Continue,
    Squiggle,
    And,
    Dollar,
    Star,
    Minus,

    SingleLineCommentStart,

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

    /// Name of the token, displayed during a syntax error
    #[must_use]
    pub fn name(&self) -> String {
        match self {
            Self::RoundParenOpen => "RoundParenOpen".to_string(),
            Self::RoundParenClose => "RoundParenClose".to_string(),
            Self::RectParenOpen => "RectParenOpen".to_string(),
            Self::RectParenClose => "RectParenClose".to_string(),
            Self::BigParenOpen => "BigParenOpen".to_string(),
            Self::BigParenClose => "BigParenClose".to_string(),
            Self::Equal => "Equal".to_string(),
            Self::True => "`true`".to_string(),
            Self::False => "`false`".to_string(),
            Self::Semicolon => "Semicolon".to_string(),
            Self::Period => "Period".to_string(),
            Self::Comma => "Comma".to_string(),
            Self::Colon => "Colon".to_string(),
            Self::Underscore => "Underscore".to_string(),
            Self::Number(_) => "Number".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Char(_) => "Char".to_string(),
            Self::Identifier(_) => "Identifier".to_string(),
            Self::Let => "`let`".to_string(),
            Self::Loop => "`loop`".to_string(),
            Self::If => "`if`".to_string(),
            Self::Else => "`else`".to_string(),
            Self::Func => "`func`".to_string(),
            Self::Extern => "`extern`".to_string(),
            Self::Return => "`return`".to_string(),
            Self::ReturnArrow => "ReturnArrow".to_string(),
            Self::Break => "`break`".to_string(),
            Self::Continue => "`continue`".to_string(),
            Self::Squiggle => "Squiggle".to_string(),
            Self::And => "And".to_string(),
            Self::Dollar => "Dollar".to_string(),
            Self::Minus => "Minus".to_string(),
            Self::Star => "Star".to_string(),
            Self::SingleLineCommentStart => "SingleLineCommentStart".to_string(),
            Self::RawASM(_) => "RawASM".to_string(),
            Self::Unknown => "UNKNOWN".to_string(),
            Self::EOF => "EOF".to_string(),
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
    fn is_alphanumeric_or_underscore_or_dot(&self) -> bool;
    fn is_alphabetic_or_underscore(&self) -> bool;
    fn is_paren(&self) -> bool;
}
impl CharCustomFuncs for char {
    fn is_alphanumeric_or_underscore(&self) -> bool {
        self.is_alphanumeric() || *self == '_'
    }
    fn is_alphanumeric_or_underscore_or_dot(&self) -> bool {
        self.is_alphanumeric() || *self == '_' || *self == '.'
    }
    fn is_alphabetic_or_underscore(&self) -> bool {
        self.is_alphabetic() || *self == '_'
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
    pub fn peek(&self, i: usize) -> &Token {
        match self
            .tokens
            .get(if self.has_started { self.i + i } else { 0 })
        {
            Some(token) => token,
            None => &self.eof,
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
    pub fn expects_identifier(&self) -> Result<&Rc<String>, CompileError> {
        if let TokenContent::Identifier(v) = &self.content {
            Ok(&v)
        } else {
            Err(CompileError::unexpected_token(
                TokenContent::Identifier(Rc::new(String::new())),
                self,
            ))
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
    fn from_operator(word: &String, i: usize) -> Option<Token> {
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
            ";" => return_token!(TokenContent::Semicolon, 1),
            "," => return_token!(TokenContent::Comma, 1),
            "." => return_token!(TokenContent::Period, 1),
            ":" => return_token!(TokenContent::Colon, 1),
            "_" => return_token!(TokenContent::Underscore, 1),
            "-" => return_token!(TokenContent::Minus, 2),
            "->" => return_token!(TokenContent::ReturnArrow, 2),
            "~" => return_token!(TokenContent::Squiggle, 1),
            "&" => return_token!(TokenContent::And, 1),
            "$" => return_token!(TokenContent::Dollar, 1),
            "*" => return_token!(TokenContent::Star, 1),
            _ => None,
        }
    }
    fn is_it_a_keyword(word: &String, i: usize) -> Option<Token> {
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
            "true" => return_token!(TokenContent::True, 1),
            "false" => return_token!(TokenContent::False, 5),
            "break" => return_token!(TokenContent::Break, 5),
            "continue" => return_token!(TokenContent::Continue, 8),
            "return" => return_token!(TokenContent::Return, 6),
            "loop" => return_token!(TokenContent::Loop, 4),
            "if" => return_token!(TokenContent::If, 2),
            "extern" => return_token!(TokenContent::Extern, 6),
            _ => None,
        }
    }
}

pub fn parse_into_tokens(source: &String, source_file_path: &String) -> Vec<Token> {
    let source_file_parent_path = Path::new(source_file_path).parent().unwrap();
    let mut tokens = Vec::<Token>::new();

    let mut word_start: usize;
    let mut chars = source.char_indices().peekable();
    let mut i = 0usize;
    let mut ch: char;
    let mut word = String::new();
    let mut alias_map = HashMap::<String, String>::new();
    word.reserve(64);
    macro_rules! next {
        () => {
            match &chars.next() {
                Some((j, c)) => {
                    i = *j;
                    ch = *c;
                }
                None => {
                    i += 1;
                    ch = '\0';
                }
            }
        };
    }
    macro_rules! peek {
        () => {
            match &chars.peek() {
                Some((_, c)) => *c,
                _ => '\0',
            }
        };
    }
    loop {
        next!();
        if ch == '/' {
            match peek!() {
                '/' => {
                    while chars.next_if(|(_, c)| *c != '\n').is_some() {}
                    next!();
                }
                '*' => {
                    let mut comment_layers = 1usize;
                    chars.next();
                    while comment_layers > 0 {
                        next!();
                        if ch == '*' {
                            next!();
                            if ch == '/' {
                                comment_layers -= 1;
                            }
                        }
                        if ch == '/' {
                            next!();
                            if ch == '*' {
                                comment_layers += 1;
                            }
                        }
                    }
                    next!();
                }
                _ => (),
            }
        }
        if ch == '\0' {
            break;
        } else if ch.is_whitespace() {
            continue;
        } else if ch.is_alphabetic_or_underscore() {
            // Identifier or keyword
            word.push(ch);
            word_start = i;
            while let Some((_, ch)) = chars.next_if(|(_, c)| c.is_alphanumeric_or_underscore()) {
                word.push(ch);
            }
            tokens.push(if let Some(t) = Token::is_it_a_keyword(&word, i) {
                t
            } else {
                Token {
                    content: TokenContent::Identifier(Rc::new(word.clone())),
                    position: word_start,
                    len: word.len(),
                }
            });
            word.clear();
        } else if ch.is_ascii_digit() {
            // Number
            word.push(ch);
            word_start = i;
            while let Some((_, ch)) = chars.next_if(|(_, c)| c.is_alphanumeric_or_underscore_or_dot()) {
                word.push(ch);
            }
            tokens.push(Token {
                content: TokenContent::Number(Rc::new(word.clone())),
                position: word_start,
                len: word.len(),
            });
            word.clear();
        } else if ch == '\"' {
            // String
            word_start = i;
            let mut len = 1usize;
            while peek!() != '\0' {
                next!();
                len += 1;
                if ch == '\"' {
                    if word.chars().next_back() != Some('\\') {
                        break;
                    }
                }
                word.push(ch);
            }
            tokens.push(Token {
                content: TokenContent::String(word.clone()),
                position: word_start,
                len,
            });
            word.clear();
        } else if ch == '\'' {
            // Char
            word_start = i;
            let mut len = 1usize;
            next!();
            let char_value = if ch == '\\' {
                next!();
                len += 1;
                match ch {
                    'n' => '\n' as u8,
                    '0' => '\0' as u8,
                    '\\' => '\\' as u8,
                    '\'' => '\'' as u8,
                    '\"' => '\"' as u8,
                    _ => panic!(),
                }
            } else {
                ch as u8
            };
            next!();
            assert!(ch == '\'');
            tokens.push(Token {
                content: TokenContent::Char(char_value),
                position: word_start,
                len,
            });
            word.clear();
        } else if ch == '#' {
            // Macro
            while let Some((_, ch)) = chars.next_if(|(_, c)| c.is_alphanumeric_or_underscore()) {
                word.push(ch);
            }
            match word.as_str() {
                "include" => {
                    // go to the next non-whitespace character
                    while chars.next_if(|(_, c)| c.is_whitespace()).is_some() {}
                    // get file name
                    word.clear();
                    while let Some((_, ch)) = chars.next_if(|(_, c)| *c != '\n') {
                        word.push(ch);
                    }
                    let joined_path = cat_path(&source_file_parent_path, &word);
                    let included_content = fs::read_to_string(joined_path.clone()).unwrap();
                    tokens.append(&mut parse_into_tokens(&included_content, &joined_path));
                }
                "alias" => {
                    // go to the next non-whitespace character
                    while chars.next_if(|(_, c)| c.is_whitespace()).is_some() {}
                    // get alias name
                    let mut name = String::new();
                    while let Some((_, ch)) = chars.next_if(|(_, c)| !c.is_whitespace()) {
                        name.push(ch);
                    }
                    let mut content = String::new();
                    while let Some((_, ch)) = chars.next_if(|(_, c)| *c != '\n') {
                        content.push(ch);
                    }
                    alias_map.insert(name, content);
                }
                id => {
                    if let Some(aliased_content) = alias_map.get(id) {
                        let mut aliased_tokens =
                            parse_into_tokens(aliased_content, source_file_path);
                        tokens.append(&mut aliased_tokens);
                    } else {
                        panic!("Cannot recogize macro keyword {id}");
                    }
                }
            }
            word.clear();
        } else {
            // Operator
            word.push(ch);
            word_start = i;
            let mut token = Token::from_operator(&word, word_start).unwrap(); // TODO: error prompt if invalid token
            word.push(peek!());
            while let Some(t) = Token::from_operator(&word, word_start) {
                chars.next();
                token = t;
                word.push(peek!());
            }
            tokens.push(token);
            word.clear();
        }
    }

    tokens
}

fn cat_path(original: &Path, tail: &String) -> String {
    original.join(tail.clone()).to_str().unwrap().to_string()
}
