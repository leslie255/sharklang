use super::tokens::*;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct CodeBlock {
    pub body: Vec<usize>,
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

    // Raw ASM
    Label(String),
    RawASM(String),

    // Control flows
    FuncDef(String, CodeBlock),
    ReturnVoid,

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
    pub is_top_level: bool,
}
#[macro_export]
macro_rules! print_ast {
    ($ast: expr) => {
        for (i, node) in $ast.iter().enumerate() {
            print!("{}:\t{:?}", i, node.expr);
            if node.is_top_level {
                print!("\ttop_level");
            }
            if node.is_root {
                print!("\troot");
            }
            println!("");
        }
    };
}

#[derive(Default)]
pub struct AST {
    pub nodes: Vec<ASTNode>,
}

impl AST {
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
            is_top_level: false,
        });
        self.nodes.last_mut().unwrap().expr = expr;
    }
}

fn parse_func_args(tokens: &mut TokenStream, tree: &mut AST) -> Vec<usize> {
    let mut token: Token;

    let mut args: Vec<usize> = Vec::new();

    // make sure there is a `(`
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
            TokenContent::RoundParenClose => break,
            _ => (),
        }
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

fn parse_single_expr(tree: &mut AST, tokens: &mut TokenStream) -> usize {
    let token = tokens.next();
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
    tree.nodes.len() - 1
}

fn parse_expression(tree: &mut AST, tokens: &mut TokenStream) -> usize {
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
                    let i = parse_single_expr(tree, tokens);
                    tree.new_expr(Expression::VarAssign(id, i));
                    return tree.nodes.len() - 1;
                }
                _ => panic!(
                    "{}:{} expecting `=` or `(` after {}",
                    token.line, token.column, id
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
            // get rhs
            let i = parse_single_expr(tree, tokens);
            tree.new_expr(Expression::VarInit(lhs, i));
            return tree.nodes.len() - 1;
        }
        TokenContent::Return => match tokens.look_ahead(1).content {
            TokenContent::Semicolon => {
                tree.new_expr(Expression::ReturnVoid);
                return tree.nodes.len() - 1;
            }
            _ => todo!(),
        },
        TokenContent::Semicolon => {
            return usize::MAX;
        }
        TokenContent::RawASM(text) => {
            tree.new_expr(Expression::RawASM(text));
            return tree.nodes.len() - 1;
        }
        _ => {
            panic!(
                "{}:{} unidentified token `{:?}`",
                token.line, token.column, token.content
            );
        }
    };
}

fn parse_top_level(tree: &mut AST, tokens: &mut TokenStream) -> usize {
    let mut token: Token;
    match tokens.look_ahead(1).content {
        TokenContent::Func => {
            tokens.next();
            token = tokens.next();
            let func_name: String;
            let mut code_block = CodeBlock::default();
            if let TokenContent::Identifier(id) = token.content {
                func_name = id;
            } else {
                panic!(
                    "{}:{} expected an identifier after `func`, found {:?}",
                    token.line, token.column, token.content
                );
            }
            // TODO: argument names
            tokens.next();
            tokens.next();

            token = tokens.next();
            if token.content != TokenContent::BigParenOpen {
                panic!(
                    "{}:{} expected `{{`, found {:?}",
                    token.line, token.column, token.content
                );
            }
            loop {
                match tokens.look_ahead(1).content {
                    TokenContent::EOF | TokenContent::BigParenClose => {
                        tokens.next();
                        break;
                    }
                    _ => (),
                }
                let i = parse_expression(tree, tokens);
                if let Some(node) = tree.nodes.get_mut(i) {
                    if node.is_root {
                        continue;
                    }
                    node.is_root = true;
                    code_block.body.push(i);
                }
            }
            let expr = Expression::FuncDef(func_name, code_block);
            tree.new_expr(expr);
            tree.nodes.last_mut().unwrap().is_root = true;
        }
        _ => {
            parse_expression(tree, tokens);
        }
    }
    tree.nodes.len().saturating_sub(1)
}

pub fn construct_ast(mut tokens: TokenStream) -> AST {
    let mut ast = AST::default();
    loop {
        parse_top_level(&mut ast, &mut tokens);
        if !ast.nodes.is_empty() {
            ast.nodes.last_mut().unwrap().is_top_level = true;
        }
        if tokens.look_ahead(1).content.is_eof() {
            break;
        }
    }
    ast
}
