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
    UInt(u64),
    SInt(i64),
    Float(f64),
    String(String),
    Char(u8),
    Identifier(String),
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

    SingleLineCommentStart,
    NewLine,

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

#[derive(Debug, Clone, Default)]
pub struct TokenStream {
    pub tokens: Vec<Token>,
    pub i: usize,

    has_started: bool,
}

impl TokenStream {
    pub fn next(&mut self) -> Token {
        let t = match self.tokens.get(if self.has_started {
            self.i += 1;
            self.i
        } else {
            self.has_started = true;
            0
        }) {
            Some(token) => token.clone(),
            None => Token::eof(),
        };
        t
    }
    pub fn look_ahead(&self, i: usize) -> Token {
        let t = match self
            .tokens
            .get(if self.has_started { self.i + i } else { 0 })
        {
            Some(token) => token.clone(),
            None => Token::eof(),
        };
        t
    }
    pub fn current(&self) -> Token {
        match self.tokens.get(if self.has_started { self.i } else { 0 }) {
            Some(token) => token.clone(),
            None => Token::eof(),
        }
    }
    pub fn skip_to_next_expr(&mut self) -> Token {
        loop {
            let token = self.next();
            if token.indicates_end_of_expr() {
                let next = self.look_ahead(1);
                if next.indicates_end_of_expr() {
                    break self.next();
                }
            }
        }
    }
    pub fn from_prototypes(prototypes: Vec<TokenPrototype>) -> TokenStream {
        let mut stream = TokenStream {
            tokens: Vec::new(),
            i: 0,
            has_started: false,
        };

        for protytype in prototypes {
            if !protytype.source.is_empty() {
                stream.tokens.push(protytype.construct_token());
            }
        }

        stream
    }
}

trait StringCustomFuncs {
    fn is_asm_instruction(&self) -> bool;
}
impl StringCustomFuncs for String {
    fn is_asm_instruction(&self) -> bool {
        match self.as_str() {
            "mov" | "push" | "pop" | "ret" | "call" | "cmp" | "add" | "sub" | "mul" | "div"
            | "extern" | "inc" | "dec" | "jmp" | "jo" | "jno" | "js" | "jns" | "je" | "jz"
            | "jne" | "jnz" | "jp" | "jpe" | "jnp" | "jpo" | "jcxz" | "jecxz" | "jb" | "jnae"
            | "jc" | "jnb" | "jae" | "jnc" | "jbe" | "jna" | "ja" | "jnbe" | "jl" | "jnge"
            | "jge" | "jnl" | "jle" | "jng" | "jg" | "jnle" | "db" | "dw" | "dq" => true,
            _ => false,
        }
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
}

#[derive(Debug, Default, Clone)]
pub struct TokenPrototype {
    source: String,
    position: usize,
    len: usize,
}
impl TokenPrototype {
    fn from(str: &String, position: usize) -> TokenPrototype {
        let mut prototype = TokenPrototype::default();
        prototype.source = str.clone();
        prototype.position = position.saturating_sub(str.len());
        prototype.len = str.len();

        prototype
    }
    fn is_valid(str: &String) -> bool {
        match str.as_str() {
            "(" | ")" | "[" | "]" | "{" | "}" | "=" | ";" | "." | "," | ":" | "->" | "~" | "_"
            | "let" | "loop" | "if" | "else" | "func" | "true" | "false" | "return" | "break"
            | "continue" | "//" | "&" | "$" | "\n" => return true,
            _ => {
                if str.is_empty() {
                    return false;
                }
                let first_ch = str.chars().nth(0).unwrap();

                // is it an assembly instruction?
                let mut first_word = String::new();
                for ch in str.chars() {
                    if !ch.is_whitespace() {
                        break;
                    }
                    first_word.push(ch);
                }
                if first_word.is_asm_instruction() {
                    return true;
                }

                // is it a number?
                if first_ch.is_numeric() || first_ch == '-' {
                    let mut dot_count: usize = 0; // number of `.`
                    for ch in str.chars().skip(1) {
                        if ch == '.' {
                            dot_count += 1;
                            if dot_count > 1 {
                                return false;
                            }
                        }
                        if !ch.is_numeric() {
                            return false;
                        }
                    }
                    return true;
                }

                // is it an identifier?
                if first_ch.is_alphanumeric_or_underscore() {
                    for ch in str.chars().skip(1) {
                        if !ch.is_alphanumeric_or_underscore() {
                            return false;
                        }
                    }
                    return true;
                }

                return false;
            }
        }
    }
    fn construct_token(&self) -> Token {
        match self.source.as_str() {
            "(" => return Token::new(TokenContent::RoundParenOpen, self.position, self.len),
            ")" => return Token::new(TokenContent::RoundParenClose, self.position, self.len),
            "[" => return Token::new(TokenContent::RectParenOpen, self.position, self.len),
            "]" => return Token::new(TokenContent::RectParenClose, self.position, self.len),
            "{" => return Token::new(TokenContent::BigParenOpen, self.position, self.len),
            "}" => return Token::new(TokenContent::BigParenClose, self.position, self.len),
            "=" => return Token::new(TokenContent::Equal, self.position, self.len),
            "true" => return Token::new(TokenContent::True, self.position, self.len),
            "false" => return Token::new(TokenContent::False, self.position, self.len),
            "let" => return Token::new(TokenContent::Let, self.position, self.len),
            "loop" => return Token::new(TokenContent::Loop, self.position, self.len),
            ";" => return Token::new(TokenContent::Semicolon, self.position, self.len),
            "." => return Token::new(TokenContent::Period, self.position, self.len),
            "," => return Token::new(TokenContent::Comma, self.position, self.len),
            ":" => return Token::new(TokenContent::Colon, self.position, self.len),
            "if" => return Token::new(TokenContent::If, self.position, self.len),
            "else" => return Token::new(TokenContent::Else, self.position, self.len),
            "func" => return Token::new(TokenContent::Func, self.position, self.len),
            "break" => return Token::new(TokenContent::Break, self.position, self.len),
            "continue" => return Token::new(TokenContent::Continue, self.position, self.len),
            "return" => return Token::new(TokenContent::Return, self.position, self.len),
            "&" => return Token::new(TokenContent::And, self.position, self.len),
            "$" => return Token::new(TokenContent::Dollar, self.position, self.len),
            "->" => return Token::new(TokenContent::ReturnArrow, self.position, self.len),
            "~" => return Token::new(TokenContent::Squiggle, self.position, self.len),
            "_" => return Token::new(TokenContent::Underscore, self.position, self.len),
            "//" => {
                return Token::new(
                    TokenContent::SingleLineCommentStart,
                    self.position,
                    self.len,
                )
            }
            "\n" => return Token::new(TokenContent::NewLine, self.position, self.len),
            _ => {
                let first_ch = self.source.chars().nth(0).unwrap();

                // is a string
                if first_ch == '\"' {
                    let mut str_content = self.source.chars();
                    str_content.next();
                    str_content.next_back();
                    return Token::new(
                        TokenContent::String(str_content.as_str().to_string()),
                        self.position,
                        self.len,
                    );
                }

                // is a character
                if first_ch == '\'' {
                    let byte: u8 = self.source.bytes().nth(1).unwrap();
                    return Token::new(TokenContent::Char(byte), self.position, self.len);
                }

                // is a floating point number
                if (first_ch.is_numeric() || first_ch == '-') && self.source.contains('.') {
                    if let Ok(float) = self.source.parse() {
                        return Token::new(TokenContent::Float(float), self.position, self.len);
                    } else {
                        panic!();
                    }
                }

                // is an unsigned integar
                if first_ch.is_numeric() {
                    if let Ok(uint) = self.source.parse() {
                        return Token::new(TokenContent::UInt(uint), self.position, self.len);
                    } else {
                        panic!();
                    }
                }

                // is a signed integar
                if first_ch == '-' {
                    if let Ok(int) = self.source.parse() {
                        return Token::new(TokenContent::SInt(int), self.position, self.len);
                    } else {
                        panic!();
                    }
                }

                // is an identifier or asm instruction
                let mut first_word = String::new();
                for ch in self.source.chars() {
                    if ch.is_whitespace() {
                        break;
                    }
                    first_word.push(ch);
                }
                if first_word.is_asm_instruction() {
                    return Token::new(
                        TokenContent::RawASM(self.source.clone()),
                        self.position,
                        self.len,
                    );
                } else {
                    return Token::new(
                        TokenContent::Identifier(self.source.clone()),
                        self.position,
                        self.len,
                    );
                }
            }
        }
    }
}

#[allow(unused, unused_assignments)]
pub fn parse_tokens(source: &String) -> TokenStream {
    let mut prototypes: Vec<TokenPrototype> = Vec::new();

    let mut current_word = String::new();
    let mut last_one_is_valid = false;

    let mut iter = source.char_indices().peekable();

    let mut i: usize = 0;
    let mut ch: char = '\0';
    let mut last_ch: char;

    macro_rules! next {
        () => {{
            last_ch = ch;
            (i, ch) = match iter.next() {
                Some(x) => x,
                None => break,
            }
        }};
    }

    loop {
        // is it a comment?
        if current_word == "//" {
            loop {
                next!();
                current_word.push(ch);
                if ch == '\n' {
                    break;
                }
            }
        }

        // is it a string literal?
        if current_word == "\"" {
            loop {
                next!();
                if ch == '\\' {
                    next!();
                    match ch {
                        'n' => current_word.push('\n'),
                        _ => current_word.push(ch),
                    }
                    continue;
                }
                current_word.push(ch);
                if ch == '\"' {
                    break;
                }
            }
            prototypes.push(TokenPrototype::from(&current_word, i));
            next!();
            last_one_is_valid = true;
            current_word = String::from(ch);
        }

        // is it a character literal?
        if current_word == "'" {
            next!();
            if !ch.is_ascii() {
                println!("character literal must be in ascii");
                std::process::exit(1);
            }
            current_word.push(ch);
            next!();
            if ch != '\'' {
                println!("character literal must be in ascii");
                std::process::exit(1);
            }
            current_word.push(ch);
            prototypes.push(TokenPrototype::from(&current_word, i));
        }

        // is it assembly instruction?
        if let Some(peek) = iter.peek() {
            if current_word.is_asm_instruction() && peek.1.is_whitespace() {
                loop {
                    next!();
                    current_word.push(ch);
                    if ch == '\n' {
                        break;
                    }
                }
                prototypes.push(TokenPrototype::from(&current_word, i));
                next!();
                last_one_is_valid = true;
                current_word = String::from(ch);
            }
        }

        if TokenPrototype::is_valid(&current_word) {
            next!();
            current_word.push(ch);
            last_one_is_valid = true;
        } else {
            if last_one_is_valid {
                let c = current_word.pop().unwrap();
                prototypes.push(TokenPrototype::from(&current_word, i));
                current_word = c.to_string();
            } else {
                next!();
                if last_ch.is_whitespace() {
                    current_word = ch.to_string();
                } else {
                    current_word.push(ch);
                }
            }
            last_one_is_valid = false;
        }
    }

    TokenStream::from_prototypes(prototypes)
}
