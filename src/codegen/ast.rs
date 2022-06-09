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
    RoundParenOpen,
    RoundParenClose,
    RectParenOpen,
    RectParenClose,
    BigParenOpen,
    BigParenClose,
    Equal,
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
        } else if self
            .value
            .chars()
            .nth(0)
            .unwrap()
            .is_alphanumeric_or_underscore()
        {
            self.class = TokenClass::Identifier;
            return;
        }

        self.class = match self.value.as_str() {
            "(" => TokenClass::RoundParenOpen,
            "[" => TokenClass::RectParenOpen,
            "{" => TokenClass::BigParenOpen,
            ")" => TokenClass::RoundParenClose,
            "]" => TokenClass::RectParenClose,
            "}" => TokenClass::BigParenClose,
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
            match tokens.last() {
                Some(token) => token.value.chars().last().unwrap_or_default(),
                None => '\0',
            }
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
        } else if ch.is_alphanumeric_or_underscore() {
            if !tokens_last!(last_char).is_alphanumeric_or_underscore() {
                new_token!();
            }
            tokens_last!().value.push(ch);
        } else if ch == tokens_last!(last_char) && !ch.is_paren() {
            tokens_last!().value.push(ch);
        } else {
            if !ch.is_whitespace() {
                new_token!();
                tokens_last!().value.push(ch);
            }
        }
        tokens_last!().determine_type();
    }
    let mut filtered: Vec<Token> = Vec::new();
    tokens.iter().for_each(|token| {
        if !token.value.is_empty() {
            filtered.push(token.clone());
        }
    });
    filtered
}

#[derive(Debug)]
pub enum Expression {
    // Non-Recursive expression
    Identifier(String),
    NumberLiteral(u64),
    StringLiteral(String),
    // Recursive expression
    FuncCall(String, Vec<Expression>), // function name, arguments
    VarInit(String, Box<Expression>),  // lhs, rhs
    VarSet(String, Box<Expression>),   // lhs, rhs

    Unknown,
}
impl Default for Expression {
    fn default() -> Self {
        return Expression::Unknown;
    }
}

pub type AST = Vec<Expression>;

#[allow(unused)] // rustc wtf?
fn recursive_parse_token(tokens_iter: &mut std::slice::Iter<Token>) -> Option<Expression> {
    let mut token: &Token;
    macro_rules! next {
        () => {
            match tokens_iter.next() {
                Some(x) => {
                    token = x;
                }
                None => {
                    return Option::None;
                }
            }
        };
    }
    next!();

    // recursive statements
    if token.value.as_str() == "(" {
        next!();
        match token.value.as_str() {
            "let" => {
                // variable initialize
                next!();
                let lhs = &token.value;
                next!(); // TODO: error if this is not an equal sign
                match recursive_parse_token(tokens_iter) {
                    Some(rhs) => {
                        next!();
                        return Option::Some(Expression::VarInit(lhs.clone(), Box::new(rhs)));
                    }
                    None => return Option::None,
                };
            }
            "set" => {
                // variable initialize
                next!();
                let lhs = &token.value;
                next!(); // TODO: error if this is not an equal sign
                match recursive_parse_token(tokens_iter) {
                    Some(rhs) => {
                        next!();
                        return Option::Some(Expression::VarSet(lhs.clone(), Box::new(rhs)));
                    }
                    None => return Option::None,
                };
            }

            _ => {
                // function call
                let name = &token.value;
                let mut args: Vec<Expression> = Vec::new();
                loop {
                    match recursive_parse_token(tokens_iter) {
                        Some(x) => args.push(x),
                        None => break,
                    };
                }
                return Option::Some(Expression::FuncCall(name.clone(), args));
            }
        }
    } else {
        // non-recursive expression
        return match token.class {
            TokenClass::Number => {
                Option::Some(Expression::NumberLiteral(token.value.parse().unwrap()))
            }
            TokenClass::String => Option::Some(Expression::StringLiteral(token.value.clone())),
            TokenClass::Identifier => Option::Some(Expression::Identifier(token.value.clone())),
            TokenClass::RoundParenClose => Option::None,
            _ => panic!("unidentified symbol {:?}", token.value),
        };
    }
}

pub fn construct_ast(source: String) -> AST {
    let tokens = parse_tokens(source);
    let mut iter = tokens.iter();
    let mut ast = AST::new();
    loop {
        match recursive_parse_token(&mut iter) {
            Some(expression) => {
                ast.push(expression);
            }
            None => {
                break;
            }
        }
    }
    ast
}
