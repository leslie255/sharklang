macro_rules! bool_toggle {
    ($value: expr) => {
        if $value {
            $value = false
        } else {
            $value = true
        }
    };
}
#[derive(Debug, PartialEq, Clone)]
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
    Period, // .
    Number,
    String,
    Identifier,

    Unknown,
}
impl Default for TokenClass {
    fn default() -> Self {
        return TokenClass::Unknown;
    }
}

#[derive(Debug, Default, Clone)]
struct Token {
    pub class: TokenClass,
    pub value: String,
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
            "." => TokenClass::Period,
            _ => self.class.clone(),
        };
    }
}

fn parse_tokens(source: String) -> Vec<Token> {
    // TODO: add line & column number for each token
    let mut tokens: Vec<Token> = Vec::new();
    macro_rules! tokens_last {
        () => {
            tokens.last_mut().unwrap()
        };
        (last_char) => {
            tokens
                .last()
                .unwrap()
                .value
                .chars()
                .last()
                .unwrap_or_default()
        };
    }
    macro_rules! new_token {
        // create a new token is the last one is empty
        () => {
            match tokens.last() {
                Some(last_token) => {
                    if !last_token.value.is_empty() {
                        tokens.push(Token::default());
                    }
                }
                None => {
                    tokens.push(Token::default());
                }
            }
        };
    }
    let mut is_inside_string = false;
    for ch in source.chars() {
        if ch == '"' {
            bool_toggle!(is_inside_string);
            if is_inside_string {
                new_token!();
                tokens_last!().class = TokenClass::String;
            }
            continue;
        }
        if is_inside_string {
            // TODO: string escape \
            tokens_last!().value.push(ch);
            continue;
        }
        if ch.is_whitespace() {
            new_token!();
        } else if ch.is_alphanumeric() {
            if !tokens_last!(last_char).is_alphanumeric() {
                new_token!();
            }
            tokens_last!().value.push(ch);
        } else if ch == tokens_last!(last_char) {
            tokens_last!().value.push(ch);
        } else {
            if !ch.is_whitespace() {
                new_token!();
                tokens_last!().value.push(ch);
            }
        }
        tokens_last!().determine_type();
    }
    tokens
}

#[derive(Debug)]
#[allow(unused)] // temporary
pub enum Expression {
    // Non-Recursive expression
    Keyword(String),
    Identifier(String),
    NumberLiteral(String),
    StringLiteral(String),
    // Recursive expression
    FuncCall(String, Vec<Expression>),

    Unknown,
}
impl Default for Expression {
    fn default() -> Self {
        return Expression::Unknown;
    }
}

pub type AST = Vec<Expression>;

fn parse_single_expression(token: &Token) -> Expression {
    match token.class {
        TokenClass::Number => Expression::NumberLiteral(token.value.parse().unwrap()),
        TokenClass::String => Expression::StringLiteral(token.value.clone()),
        TokenClass::Identifier => Expression::Identifier(token.value.clone()),
        _ => Expression::Unknown,
    }
}

fn recursive_parse_token(ast: &mut AST, tokens: Vec<Token>, i: &mut usize) {
    let mut tokens_iter = tokens.iter().skip(*i);
    let mut token: &Token;
    macro_rules! next {
        () => {
            token = match tokens_iter.next() {
                Some(x) => x,
                None => break,
            }
        };
    }
    loop {
        next!();
        if token.value == "@" {
            // function call
            next!();
            let name = &token.value;
            let mut args: Vec<Expression> = Vec::new();
            loop {
                next!();
                if token.class == TokenClass::Period {
                    break;
                }
                args.push(parse_single_expression(token));
            }
            ast.push(Expression::FuncCall(name.clone(), args));
        }
    }
}

pub fn construct_ast(source: String) -> AST {
    let tokens = parse_tokens(source);
    for token in &tokens {
        println!("{:?}", token.value);
    }
    let mut ast = AST::new();
    let mut i = 0;
    recursive_parse_token(&mut ast, tokens, &mut i);
    return ast;
}
