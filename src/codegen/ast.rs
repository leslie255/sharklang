use super::tokens::*;
use super::typecheck::*;

use std::collections::HashMap;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct CodeBlock {
    pub body: Vec<usize>,
    pub args: Vec<String>,
    pub var_addrs: HashMap<String, usize>,
    pub var_types: HashMap<String, DataType>, // also includes arguments
    pub total_var_bytes: u64,
}
impl CodeBlock {
    pub fn gen_vars(&mut self, nodes: &Vec<ASTNode>) {
        for arg in &self.args {
            self.total_var_bytes += self.var_types.get(arg).unwrap().size();
            self.var_addrs
                .insert(arg.clone(), self.total_var_bytes as usize);
        }
        let mut has_var = false;
        for i in self.body.iter() {
            has_var = true;
            let node = &nodes[*i];
            if let Expression::VarInit(var_name, var_type, _) = &node.expr {
                self.total_var_bytes += var_type.size();
                self.var_addrs
                    .insert(var_name.clone(), self.total_var_bytes as usize);
                self.var_types.insert(var_name.clone(), var_type.clone());
            }
        }
        if !has_var {
            self.total_var_bytes += 8; // stack pointer
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // Non-Recursive expression
    Identifier(String),
    NumberLiteral(u64),
    StringLiteral(String),
    // Recursive expression
    FuncCall(String, Vec<usize>),     // function name, arguments
    VarInit(String, DataType, usize), // lhs, rhs
    VarAssign(String, usize),         // lhs, rhs

    // Raw ASM
    Label(String),
    RawASM(String),

    // Control flows
    FuncDef(String, CodeBlock),
    ReturnVoid,
    ReturnVal(usize),

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
    pub func_defs: HashMap<String, usize>,
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
        match expr {
            Expression::FuncDef(ref func_name, _) => {
                self.func_defs
                    .insert(func_name.clone(), self.nodes.len() - 1);
            }
            _ => (),
        }
        self.nodes.last_mut().unwrap().expr = expr;
    }
}

fn parse_fn_call_args(tokens: &mut TokenStream, tree: &mut AST) -> Vec<usize> {
    let mut args: Vec<usize> = Vec::new();
    let mut token = tokens.next();

    // make sure there is a `(`
    if TokenContent::RoundParenOpen != token.content {
        panic!(
            "{}:{} expected a `(` for function call",
            token.line, token.column
        );
    }

    loop {
        // get the next argument
        token = tokens.next();

        // if there is a `)` coming immediately after the `(`, there are no arguments
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
                        let args = parse_fn_call_args(tokens, tree);
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
                let args = parse_fn_call_args(tokens, tree);
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

fn parse_expr(tree: &mut AST, tokens: &mut TokenStream) -> usize {
    let mut token: Token;

    token = tokens.next();

    match token.content {
        TokenContent::Identifier(id) => {
            match tokens.look_ahead(1).content {
                TokenContent::RoundParenOpen => {
                    // is a function call
                    let args = parse_fn_call_args(tokens, tree);
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
            // let var_name: type = rhs;
            // rhs could be: number literal, identifier, function call
            // get lhs
            token = tokens.next();
            let lhs: String;
            let var_type: DataType;
            if let TokenContent::Identifier(var_name) = token.content {
                lhs = var_name;
            } else {
                panic!(
                    "{}:{} expecting an identifier following `let`, found {:?}",
                    token.line, token.column, token.content
                );
            }
            // get type
            if tokens.next().content == TokenContent::Colon {
                token = tokens.next();
                if let TokenContent::Identifier(type_name) = token.content {
                    var_type = DataType::from_str(type_name.clone()).unwrap_or_else(|| {
                        panic!(
                            "{}:{} `{}` is not a valid data type",
                            token.line, token.column, type_name
                        )
                    });
                } else {
                    panic!(
                        "{}:{} `{:?}` is not a valid data type",
                        token.line, token.column, token.content
                    )
                }
            } else {
                panic!(
                    "{}:{} expecting `:` after variable name, found {:?}",
                    tokens.look_ahead(0).line,
                    tokens.look_ahead(0).column,
                    tokens.look_ahead(0).content
                );
            }
            // make sure there is an `=`
            if tokens.next().content != TokenContent::Equal {
                panic!(
                    "{}:{} expecting `=` after variable type, found {:?}",
                    tokens.look_ahead(0).line,
                    tokens.look_ahead(0).column,
                    tokens.look_ahead(0).content
                );
            }
            // get rhs
            let i = parse_single_expr(tree, tokens);
            tree.new_expr(Expression::VarInit(lhs, var_type, i));
            return tree.nodes.len() - 1;
        }
        TokenContent::Return => match tokens.look_ahead(1).content {
            TokenContent::Semicolon => {
                tree.new_expr(Expression::ReturnVoid);
                return tree.nodes.len() - 1;
            }
            _ => {
                let i = parse_single_expr(tree, tokens);
                tree.new_expr(Expression::ReturnVal(i));
                return tree.nodes.len() - 1;
            }
        },
        TokenContent::Semicolon => {
            return usize::MAX;
        }
        TokenContent::RawASM(text) => {
            tree.new_expr(Expression::RawASM(text));
            return tree.nodes.len() - 1;
        }
        TokenContent::String(str) => {
            tree.new_expr(Expression::StringLiteral(str));
            tree.new_expr(Expression::FuncCall(
                String::from("println"),
                vec![tree.nodes.len() - 1],
            ));
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

fn parse_fn_def_args(tokens: &mut TokenStream) -> Vec<(String, DataType)> {
    let mut token = tokens.next();

    // make sure there is a `(`
    if token.content != TokenContent::RoundParenOpen {
        panic!(
            "{}:{} expecting `(` after function call, found {:?}",
            token.line, token.column, token.content
        );
    }

    // if there is a `)` coming immediately after the `(`, there are no arguments
    if tokens.look_ahead(1).content == TokenContent::RoundParenClose {
        tokens.next();
        return Vec::new();
    }

    let mut args: Vec<(String, DataType)> = Vec::new();

    loop {
        let arg_name: String;
        let arg_type: DataType;

        token = tokens.next();
        match &token.content {
            TokenContent::Identifier(id) => {
                arg_name = id.clone();
            }
            _ => panic!(
                "{}:{} expecting an identifier as the name of a function arguments, found {:?}",
                token.line, token.column, token.content
            ),
        }

        token = tokens.next();
        if token.content != TokenContent::Colon {
            panic!(
                "{}:{} expecting `:` after name of a function arguments, found {:?}",
                token.line, token.column, token.content
            );
        }

        token = tokens.next();
        if let TokenContent::Identifier(type_name) = token.content {
            arg_type = DataType::from_str(type_name.clone()).unwrap_or_else(|| {
                panic!(
                    "{}:{} {} is not a valid data type",
                    token.line, token.column, type_name
                )
            });
        } else {
            panic!(
                "{}:{} expecting an identifier after `:` for argument name, found {:?}",
                token.line, token.column, token.content
            );
        }
        args.push((arg_name, arg_type));

        token = tokens.next();
        match &token.content {
            TokenContent::Comma => {}
            TokenContent::RoundParenClose => break,
            _ => panic!(
                "{}:{} expected `)` or `,` after an argument name, found {:?}",
                token.line, token.column, token.content
            ),
        }
    }

    args
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

            let func_args = parse_fn_def_args(tokens);
            for arg in func_args {
                code_block.args.push(arg.0.clone());
                code_block.var_types.insert(arg.0, arg.1);
            }

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
                let i = parse_expr(tree, tokens);
                if let Some(node) = tree.nodes.get_mut(i) {
                    if node.is_root {
                        continue;
                    }
                    node.is_root = true;
                    code_block.body.push(i);
                }
            }
            code_block.gen_vars(&tree.nodes);
            let expr = Expression::FuncDef(func_name, code_block);
            tree.new_expr(expr);
            tree.nodes.last_mut().unwrap().is_root = true;
        }
        _ => {
            parse_expr(tree, tokens);
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
