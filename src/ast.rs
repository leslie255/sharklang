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
    Comment,
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
            "//" | "/*" | "*/" => TokenClass::Comment,
            _ => TokenClass::Unknown,
        }
    }
}

fn parse_tokens(source: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![Token::default()];
    let mut i = 0;
    while i < source.chars().count() {
        let mut ch = source.chars().nth(i).unwrap();
        let current_token = tokens.last_mut().unwrap();
        if !ch.is_whitespace() {
            if current_token.value.is_empty() {
                current_token.value.push(ch);
            } else if current_token.value == "\"" {
                current_token.class = TokenClass::String;
                while ch != '\"' {
                    println!("{}", ch);
                    ch = source.chars().nth(i).unwrap();
                    if ch == '\"' {
                        break;
                    }
                    current_token.value.push(ch);
                    i += 1;
                }
                continue;
            } else if current_token
                .value
                .chars()
                .nth(current_token.value.len() - 1)
                .unwrap()
                .is_alphanumeric()
                == ch.is_alphanumeric()
            {
                if ch == ';' {
                    tokens.push(Token {
                        class: TokenClass::Semicolon,
                        value: String::from(';'),
                    });
                    tokens.push(Token::default());
                    continue;
                } else {
                    current_token.value.push(ch);
                }
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
            tokens.push(Token::default());
        }
        let last_token = &mut tokens.last_mut().unwrap();
        if last_token.value.is_empty() {
            last_token.value = String::from("");
        } else {
            last_token.determine_type();
        }
        i += 1;
    }
    tokens
}

#[derive(Debug)]
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

fn parse_non_recursive_token(tokens: Vec<Token>, i: &mut usize) -> Expression {
    macro_rules! token {
        (current) => {
            tokens.get(*i).unwrap()
        };
    }
    if token!(current).value == "\"" {
        let mut expr = Expression::StringLiteral(String::from(""));
        while *i < tokens.len() {
            if token!(current).value == "\"" {
                break;
            }
            match expr {
                Expression::StringLiteral(ref mut str) => {
                    str.push_str(&token!(current).value.clone());
                }
                _ => {}
            }
            *i += 1;
        }
        expr
    } else if token!(current).class == TokenClass::Number {
        let num = token!(current).value.parse().unwrap_or_default();
        *i += 1;
        Expression::NumberLiteral(num)
    } else {
        *i += 1;
        Expression::Unknown
    }
}

fn recursive_parse_expr(ast: &mut AST, tokens: Vec<Token>, i: &mut usize) {
    let default_token = Token::default();
    macro_rules! token {
        (current) => {
            tokens.get(*i).unwrap()
        };
        ($i: expr) => {
            tokens.get($i).unwrap_or(&default_token)
        };
    }
    while *i < tokens.len() {
        if token!(*i + 1).value == String::from("(")
            && token!(current).class == TokenClass::Identifier
        {
            let expr = Expression::FuncCall(token!(current).value.clone(), {
                *i += 2;
                let mut args: Vec<Expression> = Vec::new();
                while *i < tokens.len() {
                    if token!(current).value == String::from(")") {
                        break;
                    } else if token!(current).value == String::from(",") {
                        *i += 1;
                        continue;
                    }
                    args.push(parse_non_recursive_token(tokens.clone(), i));
                }
                args
            });
            ast.push(expr);
        }
        *i += 1;
    }
}

pub fn construct_ast(source: String) -> AST {
    let tokens = parse_tokens(source);
    let mut ast: AST = Vec::new();
    let mut i = 0;
    recursive_parse_expr(&mut ast, tokens, &mut i);
    ast
}
