#![allow(unused)] // temporary
use super::tokens::*;

use std::collections::HashMap;

#[allow(unused_macros)]
#[macro_export]
macro_rules! print_ast {
    ($ast: expr) => {
        for (i, node) in $ast.iter().enumerate() {
            println!("{}: {:?}", i, node);
        }
    };
}

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
    FuncCall(String, Vec<usize>), // function name, arguments
    VarInit(String, usize),       // lhs, rhs
    VarSet(String, usize),        // lhs, rhs

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

fn recursive_construct_ast(
    tree: &mut AST,
    current: usize,
    tokens_iter: &mut std::slice::Iter<Token>,
) -> (usize, bool) {
    // return value 0: index of newly added expression, 1: should continue recursion
    let mut token: &Token;
    macro_rules! next {
        () => {
            match tokens_iter.next() {
                Some(x) => {
                    token = x;
                }
                None => return (0, false),
            }
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

    match &token.content {
        TokenContent::Let => {
            next!();
            if let TokenContent::Identifier(var_name) = &token.content {
                next!();
                if TokenContent::Equal != token.content {
                    panic!(
                        "{}:{} expecting `=` after variable name, found {:?}",
                        token.line, token.column, token.content
                    );
                }
                let (rhs, should_continue) = recursive_construct_ast(tree, current, tokens_iter);
                match tree.get(rhs).unwrap().expr {
                    Expression::NumberLiteral(_) | Expression::StringLiteral(_) => {
                        next!();
                        if TokenContent::Semicolon != token.content {
                            panic!(
                                "{}:{} expecting `;` in the end of `let` statement",
                                token.line, token.column
                            );
                        }
                    }
                    Expression::FuncCall(_, _) | Expression::Identifier(_) => {}
                    _ => panic!("{}:{} invalid rhs for `let`", token.line, token.column),
                }
                new_node_from_expr!(Expression::VarInit(var_name.clone(), rhs));
                return (tree.len() - 1, should_continue);
            } else {
                panic!(
                    "{}:{} expecting an identifier following `let`, found {:?}",
                    token.line, token.column, token.content
                );
            }
        }
        TokenContent::UInt(uint) => {
            new_node_from_expr!(Expression::NumberLiteral(*uint));
            return (tree.len() - 1, true);
        }
        TokenContent::String(str) => {
            new_node_from_expr!(Expression::StringLiteral(str.clone()));
            return (tree.len() - 1, true);
        }
        TokenContent::Identifier(id) => {
            next!();
            match token.content {
                TokenContent::Equal => {
                    // variable set
                    let (rhs, should_continue) =
                        recursive_construct_ast(tree, current, tokens_iter);
                    match tree.get(rhs).unwrap().expr {
                        Expression::NumberLiteral(_) | Expression::StringLiteral(_) => {
                            next!();
                            if TokenContent::Semicolon != token.content {
                                panic!(
                                    "{}:{} expecting `;` in the end of `let` statement",
                                    token.line, token.column
                                );
                            }
                        }
                        Expression::FuncCall(_, _) | Expression::Identifier(_) => {}
                        _ => panic!("{}:{} invalid rhs for `let`", token.line, token.column),
                    }
                    new_node_from_expr!(Expression::VarSet(id.clone(), rhs));
                    return (tree.len() - 1, should_continue);
                }
                TokenContent::Semicolon => {
                    new_node_from_expr!(Expression::Identifier(id.clone()));
                    return (tree.len() - 1, true);
                }
                TokenContent::RoundParenOpen => {
                    // function call
                    let mut args: Vec<usize> = Vec::new();
                    loop {
                        let (arg, should_continue) =
                            recursive_construct_ast(tree, current, tokens_iter);
                        if !should_continue {
                            panic!(
                                "{}:{} expecting a function argument, found EOF",
                                token.line, token.column
                            );
                        }
                        args.push(arg);
                        next!();
                        if TokenContent::Comma == token.content {
                        } else if TokenContent::RoundParenClose == token.content {
                            break;
                        } else {
                            panic!(
                                "{}:{} expecting an function argument, found `(`",
                                token.line, token.column
                            );
                        }
                    }
                    new_node_from_expr!(Expression::FuncCall(id.clone(), args));
                    return (tree.len() - 1, true);
                }
                _ => panic!(
                    "{}:{} expecting `(`, `;` or `=` after {}",
                    token.line, token.column, id
                ),
            }
        }
        TokenContent::Semicolon => {}
        _ => {
            panic!(
                "{}:{} unidentified token `{:?}`",
                token.line, token.column, token.content
            );
        }
    };

    (0, true)
}

pub fn flatten_ast(
    old: &AST,
    iter: &mut std::slice::Iter<ASTNode>,
    new: &mut AST,
    index_changes: &mut HashMap<usize, usize>,
) {
}

pub fn construct_ast(source: String) -> AST {
    let tokens = parse_tokens(source);
    let mut iter = tokens.iter();
    let mut ast = AST::new();
    loop {
        if !recursive_construct_ast(&mut ast, u64_max!(), &mut iter).1 {
            break;
        }
    }
    ast
}
