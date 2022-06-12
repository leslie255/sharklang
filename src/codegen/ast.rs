macro_rules! bool_toggle {
    ($value: expr) => {
        if $value {
            $value = false
        } else {
            $value = true
        }
    };
}

macro_rules! u64_max {
    () => {
        9223372036854775807
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

#[derive(Debug, Clone)]
pub enum Expression {
    // Non-Recursive expression
    Identifier(String),
    NumberLiteral(u64),
    StringLiteral(String),
    // Recursive expression
    FuncCall(String, Vec<usize>),            // function name, arguments
    VarInit(String, usize),                  // lhs, rhs
    VarSet(String, usize),                   // lhs, rhs
    VarInitFunc(String, String, Vec<usize>), // init a var from the result of a function call (lhs, function name, arguments) (won't be used until ast is flattened)

    Unknown,
}
impl Default for Expression {
    fn default() -> Self {
        return Expression::Unknown;
    }
}
#[derive(Default, Clone)]
pub struct ASTNode {
    pub expr: Expression,
    pub parent: usize, // using u64_max!() as the parent means it's a root node
}
pub type AST = Vec<ASTNode>;
impl ASTNode {
    pub fn is_recursive_type(&self) -> bool {
        match self.expr {
            Expression::VarInit(_, _) => true,
            Expression::VarSet(_, _) => true,
            Expression::FuncCall(_, _) => true,
            _ => false,
        }
    }
    pub fn is_recursive(&self, tree: &AST) -> bool {
        match &self.expr {
            Expression::VarInit(_, rhs) => tree.get(*rhs).unwrap().is_recursive_type(),
            Expression::VarSet(_, rhs) => tree.get(*rhs).unwrap().is_recursive_type(),
            Expression::FuncCall(_, args) => {
                for arg in args.iter() {
                    if tree.get(*arg).unwrap().is_recursive_type() {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }
}

impl std::fmt::Debug for ASTNode {
    fn fmt(&self, format: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.expr.fmt(format)
    }
}

#[allow(unused_assignments)] // rustc wtf??
fn recursive_parse_token(
    tree: &mut AST,
    current: usize,
    tokens_iter: &mut std::slice::Iter<Token>,
) -> Option<usize> {
    let mut token: &Token;
    macro_rules! next {
        () => {
            match tokens_iter.next() {
                Some(x) => {
                    token = x;
                }
                None => return None,
            }
        };
    }
    macro_rules! new_node {
        ($node: expr) => {
            tree.push($node)
        };
    }
    macro_rules! new_node_from_expr {
        ($expression: expr) => {
            tree.push(ASTNode {
                expr: $expression,
                parent: current,
            })
        };
    }
    next!();

    // recursive statement
    if token.value.as_str() == "(" {
        next!();
        match token.value.as_str() {
            "let" => {
                // variable initilize
                next!();
                let lhs = &token.value;
                next!(); // TODO: error if this is not an equal sign
                match recursive_parse_token(tree, current, tokens_iter) {
                    Some(rhs) => {
                        next!();
                        new_node_from_expr!(Expression::VarInit(lhs.clone(), rhs));
                        Some(tree.len() - 1)
                    }
                    None => panic!("unexpected token: {:?}", &token.value),
                }
            }
            "set" => {
                // variable set
                next!();
                let lhs = &token.value;
                next!(); // TODO: error if this is not an equal sign
                match recursive_parse_token(tree, current, tokens_iter) {
                    Some(rhs) => {
                        next!();
                        new_node_from_expr!(Expression::VarSet(lhs.clone(), rhs));
                        Some(tree.len() - 1)
                    }
                    None => panic!("unexpected token: {:?}", &token.value),
                }
            }
            _ => {
                // function call
                let name = &token.value;
                let mut args: Vec<usize> = Vec::new();
                loop {
                    match recursive_parse_token(tree, current, tokens_iter) {
                        Some(arg) => args.push(arg),
                        None => break,
                    }
                }
                new_node_from_expr!(Expression::FuncCall(name.clone(), args));
                Some(tree.len() - 1)
            }
        }
    } else {
        if token.value == ")" {
            return None;
        }
        new_node!(ASTNode {
            expr: match token.class {
                TokenClass::Number => Expression::NumberLiteral(token.value.parse().unwrap()),
                TokenClass::String => Expression::StringLiteral(token.value.clone()),
                TokenClass::Identifier => Expression::Identifier(token.value.clone()),
                _ => panic!("unexpected token: {:?}", token.value),
            },
            parent: current,
        });
        Some(tree.len() - 1)
    }
}

pub fn construct_ast(source: String) -> AST {
    let tokens = parse_tokens(source);
    let mut iter = tokens.iter();
    let mut ast = AST::new();
    loop {
        match recursive_parse_token(&mut ast, u64_max!(), &mut iter) {
            None => break,
            _ => {}
        }
    }
    ast
}
