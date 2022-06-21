use super::tokens::*;

#[allow(unused_macros)]
#[macro_export]
macro_rules! print_ast {
    ($ast: expr) => {
        for (i, node) in $ast.iter().enumerate() {
            println!("{}:\t{:?}", i, node);
        }
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
    VarAssign(String, usize),     // lhs, rhs

    Label(String),

    RawASM(String),

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
    pub is_root: bool,
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
    pub fn new_expr(&mut self, expr: Expression) {
        self.nodes.push(ASTNode {
            expr: Expression::Unknown,
            is_root: false,
        });
        self.nodes.last_mut().unwrap().expr = expr;
    }
}

impl std::fmt::Debug for ASTNode {
    fn fmt(&self, format: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.expr.fmt(format)
    }
}

fn parse_func_args(tokens: &mut TokenStream, tree: &mut AST) -> Vec<usize> {
    let mut token: Token;

    let mut args: Vec<usize> = Vec::new();

    // make sure there is an `(`
    token = tokens.next();
    if TokenContent::RoundParenOpen != token.content {
        panic!(
            "{}:{} expected a `(` for function call",
            token.line, token.column
        );
    }

    loop {
        // get the next argument
        token = tokens.next();
        match &token.content {
            TokenContent::UInt(num) => {
                tree.new_expr(Expression::NumberLiteral(*num));
            }
            TokenContent::String(str) => {
                tree.new_expr(Expression::StringLiteral(str.clone()));
            }
            TokenContent::Identifier(id) => {
                match tokens.look_ahead(1).content {
                    TokenContent::Comma | TokenContent::RoundParenClose => {
                        // is a variable
                        tree.new_expr(Expression::Identifier(id.clone()));
                    }
                    TokenContent::RoundParenOpen => {
                        // is a function call
                        let args = parse_func_args(tokens, tree);
                        tree.new_expr(Expression::FuncCall(id.clone(), args));
                    }
                    TokenContent::Equal => panic!(
                        "{}:{} variable assignment cannot be used as an argument for a function",
                        tokens.look_ahead(1).line,
                        tokens.look_ahead(1).column
                    ),
                    _ => panic!(
                        "{}:{} expecting `(`, `,` or `)`, found {:?}",
                        tokens.look_ahead(1).line,
                        tokens.look_ahead(1).column,
                        tokens.look_ahead(1).content
                    ),
                }
            }
            _ => panic!(
                "{}:{} cannot use {:?} as an argument for function",
                token.line, token.column, token.content
            ),
        }
        args.push(tree.nodes.len() - 1);
        // if next token is `,` it means there's another argument
        // if it's `)` then it means function call should end
        token = tokens.next();
        match &token.content {
            TokenContent::Comma => {}
            TokenContent::RoundParenClose => break,
            _ => panic!(
                "{}:{} expected `)` or `,` after a function argument, found {:?}",
                token.line, token.column, token.content
            ),
        }
    }

    args
}

fn parse(tree: &mut AST, tokens: &mut TokenStream) -> usize {
    // return value 0: index of newly added expression, 1: should continue recursion
    let mut token: Token;

    token = tokens.next();

    match token.content {
        TokenContent::Identifier(id) => {
            match tokens.look_ahead(1).content {
                TokenContent::RoundParenOpen => {
                    // is a function call
                    let args = parse_func_args(tokens, tree);
                    tree.new_expr(Expression::FuncCall(id.clone(), args));
                    return tree.nodes.len() - 1;
                }
                _ => {}
            }
            token = tokens.next();
            match token.content {
                TokenContent::Colon => {
                    tree.new_expr(Expression::Label(id));
                    return tree.nodes.len() - 1;
                }
                TokenContent::Equal => {
                    // is variable assign
                    token = tokens.next();
                    match token.content {
                        TokenContent::UInt(uint) => {
                            tree.new_expr(Expression::NumberLiteral(uint));
                        }
                        TokenContent::String(str) => {
                            tree.new_expr(Expression::StringLiteral(str));
                        }
                        TokenContent::Identifier(id) => match tokens.look_ahead(1).content {
                            TokenContent::RoundParenOpen => {
                                // is a function call
                                let args = parse_func_args(tokens, tree);
                                tree.new_expr(Expression::FuncCall(id, args));
                            }
                            TokenContent::Semicolon => {
                                // is a variable
                                tokens.next();
                                tree.new_expr(Expression::Identifier(id));
                            }
                            _ => panic!(
                                "{}:{} expecting `(` or `;`, found {:?}",
                                tokens.look_ahead(1).line,
                                tokens.look_ahead(1).column,
                                tokens.look_ahead(1).content
                            ),
                        },
                        _ => panic!(
                            "{}:{} `{:?}` is not a valid rhs for variable assignment",
                            token.line, token.column, token.content
                        ),
                    }
                    tree.new_expr(Expression::VarAssign(id, tree.nodes.len() - 1));
                }
                _ => panic!(
                    "{}:{} expecting `=` or `(` after {:?}",
                    token.line, token.column, token.content
                ),
            }
        }
        TokenContent::Let => {
            // get lhs
            token = tokens.next();
            let lhs: String;
            if let TokenContent::Identifier(var_name) = token.content {
                lhs = var_name;
            } else {
                panic!(
                    "{}:{} expecting an identifier following `let`, found {:?}",
                    token.line, token.column, token.content
                );
            }
            // make sure there is an `=`
            if TokenContent::Equal != tokens.next().content {
                panic!(
                    "{}:{} expecting `=` after variable name, found {:?}",
                    tokens.look_ahead(0).line,
                    tokens.look_ahead(0).column,
                    tokens.look_ahead(0).content
                );
            }
            // determine the type and get rhs
            match tokens.next().content {
                TokenContent::UInt(num) => {
                    tree.new_expr(Expression::NumberLiteral(num));
                }
                TokenContent::String(str) => {
                    tree.new_expr(Expression::StringLiteral(str));
                }
                TokenContent::Identifier(id) => match tokens.look_ahead(1).content {
                    TokenContent::RoundParenOpen => {
                        // is a function call
                        let args = parse_func_args(tokens, tree);
                        tree.new_expr(Expression::FuncCall(id, args));
                    }
                    TokenContent::Semicolon => {
                        // is a variable
                        tokens.next();
                        tree.new_expr(Expression::Identifier(id));
                    }
                    _ => panic!(
                        "{}:{} expecting `(` or `;`, found {:?}",
                        tokens.look_ahead(1).line,
                        tokens.look_ahead(1).column,
                        tokens.look_ahead(1).content
                    ),
                },
                _ => panic!("{}:{} invalid rhs for `let`", token.line, token.column),
            }
            tree.new_expr(Expression::VarInit(lhs, tree.nodes.len() - 1));
            return tree.nodes.len() - 1;
        }
        TokenContent::Semicolon => {}
        TokenContent::RawASM(text) => {
            tree.new_expr(Expression::RawASM(text));
        }
        _ => {
            panic!(
                "{}:{} unidentified token `{:?}`",
                token.line, token.column, token.content
            );
        }
    };

    0
}

pub fn construct_ast(mut tokens: TokenStream) -> AST {
    let mut ast = AST::default();
    loop {
        let i = parse(&mut ast, &mut tokens);
        ast.nodes.last_mut().unwrap().is_root = true;
        if tokens.look_ahead(1).content.is_eof() {
            break;
        }
        ast.root_nodes.push(i);
    }
    ast
}
