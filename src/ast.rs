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

trait CharCustomFuncs {
    fn is_alphanumeric_or_underscore(&self) -> bool;
}
impl CharCustomFuncs for char {
    fn is_alphanumeric_or_underscore(&self) -> bool {
        self.is_alphanumeric() || *self == '_'
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
        } else if self.value.chars().nth(0).unwrap().is_alphanumeric_or_underscore() {
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
        } else if ch.is_alphanumeric_or_underscore() {
            if !tokens_last!(last_char).is_alphanumeric_or_underscore() {
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

fn recursive_parse_token(tokens_iter: &mut std::slice::Iter<Token>) -> Option<Expression> {
    let mut token: &Token;
    macro_rules! next {
        () => {
            match tokens_iter.next() {
                Some(x) => token = x,
                None => {
                    return Option::None;
                }
            }
        };
    }
    next!();
    if token.value == "@" {
        // function call
        next!();
        let name = &token.value;
        let mut args: Vec<Expression> = Vec::new();
        loop {
            if token.value == "." {
                println!("yes");
                break;
            }
            let arg = recursive_parse_token(tokens_iter);
            match arg {
                Some(x) => args.push(x),
                None => break,
            }
        }
        return Option::Some(Expression::FuncCall(name.clone(), args));
    }
    // non-recursive expression
    match token.class {
        TokenClass::Number => Option::Some(Expression::NumberLiteral(token.value.parse().unwrap())),
        TokenClass::String => Option::Some(Expression::StringLiteral(token.value.clone())),
        TokenClass::Identifier => Option::Some(Expression::Identifier(token.value.clone())),
        _ => Option::None,
    }
}

pub fn construct_ast(source: String) -> AST {
    let tokens = parse_tokens(source);
    let mut iter = tokens.iter();
    let mut ast = AST::new();
    loop {
        match recursive_parse_token(&mut iter) {
            Some(expression) => ast.push(expression),
            None => break,
        }
    }
    ast
}
