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
    SInt(i64),
    Float(f64),
    String(String),
    Identifier(String),
    Let,
    Func,
    Return,
    ReturnArrow,

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
        match self
            .tokens
            .get(if self.has_started { self.i + i } else { 0 })
        {
            Some(token) => token.clone(),
            None => Token::eof(),
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
            | "jge" | "jnl" | "jle" | "jng" | "jg" | "jnle" => true,
            _ => false,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Token {
    pub content: TokenContent,
    pub position: usize,
}

impl Token {
    fn new(content: TokenContent, position: usize) -> Token {
        let mut token = Token::default();
        token.content = content;
        token.position = position;
        return token;
    }
    fn eof() -> Token {
        Token {
            content: TokenContent::EOF,
            position: 0,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct TokenPrototype {
    source: String,
    position: usize,
}
impl TokenPrototype {
    fn from(str: &String, position: usize) -> TokenPrototype {
        let mut prototype = TokenPrototype::default();
        prototype.source = str.clone();
        prototype.position = position;

        prototype
    }
    fn is_valid(str: &String) -> bool {
        match str.as_str() {
            "(" | ")" | "[" | "]" | "{" | "}" | "=" | ";" | "." | "," | ":" | "->" | "let"
            | "func" | "true" | "false" | "return" | "//" | "\n" => return true,
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
            "(" => return Token::new(TokenContent::RoundParenOpen, self.position),
            ")" => return Token::new(TokenContent::RoundParenClose, self.position),
            "[" => return Token::new(TokenContent::RectParenOpen, self.position),
            "]" => return Token::new(TokenContent::RectParenClose, self.position),
            "{" => return Token::new(TokenContent::BigParenOpen, self.position),
            "}" => return Token::new(TokenContent::BigParenClose, self.position),
            "=" => return Token::new(TokenContent::Equal, self.position),
            "true" => return Token::new(TokenContent::True, self.position),
            "false" => return Token::new(TokenContent::False, self.position),
            "let" => return Token::new(TokenContent::Let, self.position),
            ";" => return Token::new(TokenContent::Semicolon, self.position),
            "." => return Token::new(TokenContent::Period, self.position),
            "," => return Token::new(TokenContent::Comma, self.position),
            ":" => return Token::new(TokenContent::Colon, self.position),
            "func" => return Token::new(TokenContent::Func, self.position),
            "return" => return Token::new(TokenContent::Return, self.position),
            "->" => return Token::new(TokenContent::ReturnArrow, self.position),
            "//" => return Token::new(TokenContent::SingleLineCommentStart, self.position),
            "\n" => return Token::new(TokenContent::NewLine, self.position),
            _ => {
                let first_ch = self.source.chars().nth(0).unwrap();

                // is a string
                if first_ch == '\"' {
                    let mut str_content = String::new();
                    for ch in self.source.chars().skip(1) {
                        if ch == '\"' {
                            break;
                        }
                        str_content.push(ch);
                    }
                    return Token::new(TokenContent::String(str_content), self.position);
                }

                // is a floating point number
                if (first_ch.is_numeric() || first_ch == '-') && self.source.contains('.') {
                    if let Ok(float) = self.source.parse() {
                        return Token::new(TokenContent::Float(float), self.position);
                    } else {
                        panic!();
                    }
                }

                // is an unsigned integar
                if first_ch.is_numeric() {
                    if let Ok(uint) = self.source.parse() {
                        return Token::new(TokenContent::UInt(uint), self.position);
                    } else {
                        panic!();
                    }
                }

                // is a signed integar
                if first_ch == '-' {
                    if let Ok(int) = self.source.parse() {
                        return Token::new(TokenContent::SInt(int), self.position);
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
                    return Token::new(TokenContent::RawASM(self.source.clone()), self.position);
                } else {
                    return Token::new(
                        TokenContent::Identifier(self.source.clone()),
                        self.position,
                    );
                }
            }
        }
    }
}

#[allow(unused, unused_assignments)]
pub fn parse_tokens(source: String) -> TokenStream {
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
            // TODO: string escape codes
            loop {
                next!();
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

    for prototype in &prototypes {
        println!("{:?}", prototype.source);
    }

    TokenStream::from_prototypes(prototypes)
}
