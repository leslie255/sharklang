use super::tokens::*;

static mut LAST_RAND: u64 = 0;
pub fn quick_rand(str: &str) -> u64 {
    let mut hash: u64 = 0;
    for ch in str.chars() {
        hash += (ch as u64) * (ch as u64);
    }
    unsafe {
        hash = hash.overflowing_add(LAST_RAND).0;
        LAST_RAND = hash;
    }
    hash
}

macro_rules! u64_max {
    () => {
        9223372036854775807
    };
}

#[derive(Debug, Clone, PartialEq)]
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
    Null, // during flattening the AST some expressions will be removed from the stack
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
fn recursive_construct_ast(
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
                match recursive_construct_ast(tree, current, tokens_iter) {
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
                match recursive_construct_ast(tree, current, tokens_iter) {
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
                    match recursive_construct_ast(tree, current, tokens_iter) {
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

pub fn flatten_ast(old: &AST, iter: &mut std::slice::Iter<ASTNode>, new: &mut AST) {
    let mut node: &ASTNode;
    macro_rules! next {
        () => {
            match iter.next() {
                Some(x) => node = x,
                None => return,
            }
        };
    }
    loop {
        next!();
        if !node.is_recursive(old) {
            new.push(node.clone());
            continue;
        }
        match &node.expr {
            Expression::FuncCall(name, args) => {
                let mut new_args: Vec<usize> = Vec::new();
                for arg in args {
                    if old.get(*arg).unwrap().is_recursive(old) {
                        flatten_ast(old, iter, new);
                    }
                    if !old.get(*arg).unwrap().is_recursive_type() {
                        new_args.push(*arg);
                        continue;
                    }
                    // the last added FuncCall statement won't be needed
                    new.last_mut().unwrap().expr = Expression::Null;
                    // add a new VarInitFunc(...) before this FuncCall
                    let var_name = format!("temp_{}", quick_rand(name.as_str()));
                    new_args.push(new.len());
                    let func_name;
                    let func_args: &Vec<usize>;
                    match &old.get(*arg).unwrap().expr {
                        Expression::FuncCall(name, args) => {
                            func_name = name;
                            func_args = args;
                        }
                        _ => panic!("unexpected variable initialization syntax"),
                    }
                    new.push(ASTNode {
                        expr: Expression::VarInitFunc(
                            var_name.clone(),
                            func_name.clone(),
                            func_args.clone(),
                        ),
                        parent: node.parent,
                    });
                }
                new.push(ASTNode {
                    expr: Expression::FuncCall(name.clone(), new_args),
                    parent: node.parent,
                });
            }
            Expression::VarInit(lhs, rhs) => {
                new.last_mut().unwrap().expr = Expression::Null;
                // should be a VarInitFunc
                let func_name;
                let func_args: &Vec<usize>;
                match &old.get(*rhs).unwrap().expr {
                    Expression::FuncCall(name, args) => {
                        func_name = name;
                        func_args = args;
                    }
                    _ => panic!("unexpected variable initialization syntax"),
                }
                new.push(ASTNode {
                    expr: Expression::VarInitFunc(
                        lhs.clone(),
                        func_name.clone(),
                        func_args.clone(),
                    ),
                    parent: node.parent,
                });
            }
            _ => {}
        }
    }
}

pub fn construct_ast(source: String) -> AST {
    let tokens = parse_tokens(source);
    let mut iter = tokens.iter();
    let mut ast = AST::new();
    loop {
        match recursive_construct_ast(&mut ast, u64_max!(), &mut iter) {
            None => break,
            _ => {}
        }
    }
    ast
}
