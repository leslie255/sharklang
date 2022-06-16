use super::tokens::*;

#[allow(unused_macros)]
#[macro_export]
macro_rules! print_ast {
    ($ast: expr) => {
        for (i, node) in $ast.iter().enumerate() {
            println!("{}: {:?}", i, node);
        }
    };
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
}
impl Default for Expression {
    fn default() -> Self {
        return Expression::Unknown;
    }
}
#[derive(Default, Clone)]
pub struct ASTNode {
    pub expr: Expression,
}
#[derive(Default)]
pub struct AST {
    pub nodes: Vec<ASTNode>,
    pub root_nodes: Vec<usize>,
}

impl AST {
    #[allow(dead_code)]
    pub fn node(&self, i: usize) -> &ASTNode {
        unsafe { self.nodes.get_unchecked(i) }
    }
    pub fn expr(&self, i: usize) -> &Expression {
        unsafe { &self.nodes.get_unchecked(i).expr }
    }
}

impl std::fmt::Debug for ASTNode {
    fn fmt(&self, format: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.expr.fmt(format)
    }
}

fn recursive_construct_ast(
    tree: &mut Vec<ASTNode>,
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
            tree.push(ASTNode { expr: $expression })
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

pub fn construct_ast(tokens: &Vec<Token>) -> AST {
    let mut iter = tokens.iter();
    let mut ast = AST::default();
    loop {
        let (rhs, should_continue) = recursive_construct_ast(&mut ast.nodes, u64_max!(), &mut iter);
        if !should_continue {
            break;
        }
        ast.root_nodes.push(rhs);
    }
    ast
}
