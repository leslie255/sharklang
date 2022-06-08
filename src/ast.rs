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
            _ => self.class.clone(),
        };
    }
}

fn parse_tokens(source: String) -> Vec<Token> {
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
    tokens.push(Token::default());
    let mut is_inside_string = false;
    for ch in source.chars() {
        if ch == '"' {
            bool_toggle!(is_inside_string);
            if is_inside_string {
                tokens.push(Token::default());
                tokens_last!().class = TokenClass::String;
            }
            continue;
        }
        if is_inside_string {
            // TODO: string escape \
            tokens_last!().value.push(ch);
            continue;
        }
        if ch.is_alphanumeric() {
            if !tokens_last!(last_char).is_alphanumeric() {
                tokens.push(Token::default());
            }
            tokens_last!().value.push(ch);
        } else if ch == tokens_last!(last_char) {
            tokens_last!().value.push(ch);
        } else {
            if !ch.is_whitespace() {
                tokens.push(Token::default());
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

#[allow(unused)] // temporary
fn parse_non_recursive_token(tokens: Vec<Token>, i: &mut usize) -> Expression {
    macro_rules! token {
        (current) => {
            tokens.get(*i).unwrap()
        };
    }
    if token!(current).class == TokenClass::Number {
        let num = token!(current).value.parse().unwrap_or_default();
        *i += 1;
        Expression::NumberLiteral(num)
    } else {
        *i += 1;
        Expression::Unknown
    }
}

fn _recursive_parse_expr(ast: &mut AST, tokens: Vec<Token>, i: &mut usize) {
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
    for token in tokens {
        println!("{:?}\n{:?}", token.value, token.class);
    }
    //    let mut ast = AST::new();
    //    let mut i = 0;
    //    recursive_parse_expr(&mut ast, tokens, &mut i);
    return AST::new();
}
