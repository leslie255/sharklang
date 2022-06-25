use super::error::*;
use super::tokens::*;
use super::typecheck::*;
use std::process::exit;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct VarInfo {
    pub addr: u64,
    pub data_type: DataType,
    pub def_i: usize, // where in the AST the variable is defined
    pub is_arg: bool,
}
impl VarInfo {
    pub fn new(addr: u64, data_type: DataType, def_i: usize, is_arg: bool) -> VarInfo {
        let mut info = VarInfo {
            addr: 0,
            data_type: DataType::UInt64,
            def_i: 0,
            is_arg: false,
        };
        info.addr = addr;
        info.data_type = data_type;
        info.def_i = def_i;
        info.is_arg = is_arg;

        info
    }
}
#[derive(Debug, Clone, Default, PartialEq)]
pub struct CodeBlock {
    pub body: Vec<usize>,
    pub vars: HashMap<String, VarInfo>,
    pub stack_depth: u64,
    pub arg_types: Vec<DataType>,
    pub has_vars: bool,
}
impl CodeBlock {
    pub fn gen_vars_with_args(&mut self, nodes: &Vec<ASTNode>, args: Vec<(String, DataType)>) {
        self.has_vars = false; // has at least one variable
        for (arg_name, arg_type) in &args {
            self.has_vars = true;
            self.stack_depth += arg_type.size();
            self.vars.insert(
                arg_name.clone(),
                VarInfo::new(self.stack_depth, arg_type.clone(), 0, true),
            );
            self.arg_types.push(arg_type.clone());
        }
        for i in &self.body {
            let node = &nodes[*i];
            if let Expression::VarInit(var_name, var_type, _) = &node.expr {
                self.has_vars = true;
                self.stack_depth += var_type.size();
                self.vars.insert(
                    var_name.clone(),
                    VarInfo::new(self.stack_depth, var_type.clone(), *i, false),
                );
            }
        }
        self.stack_depth += 8;
        if !self.stack_depth.is_power_of_two() {
            let mut i = 3;
            loop {
                let stack_size = (2 as u64).pow(i);
                if self.stack_depth < stack_size {
                    self.stack_depth = stack_size;
                    break;
                }
                i += 1;
            }
        }
    }
    pub fn var_addr(&self, var_name: &String) -> Option<u64> {
        match self.vars.get(var_name) {
            Some(x) => Some(x.addr),
            None => None,
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
        panic!("{} expected a `(` for function call", token.position);
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
                        "{} variable assignment cannot be used as an argument for a function",
                        tokens.look_ahead(1).position,
                    ),
                    _ => panic!(
                        "{} expecting `(`, `,` or `)`, found {:?}",
                        tokens.look_ahead(1).position,
                        tokens.look_ahead(1).content
                    ),
                }
            }
            _ => panic!(
                "{} cannot use {:?} as an argument for function",
                token.position, token.content
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
                "{} expected `)` or `,` after a function argument, found {:?}",
                token.position, token.content
            ),
        }
    }

    args
}

fn parse_single_expr(
    tree: &mut AST,
    tokens: &mut TokenStream,
    err_collector: &mut ErrorCollector,
) -> usize {
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
            _ => {
                let token = tokens.next();
                err_collector.syntax_err(
                    &token,
                    format!("expecting `(` or `;`, found {:?}", token.content),
                );
                err_collector.print_errs();
                exit(1);
            }
        },
        _ => {
            err_collector.syntax_err(
                &token,
                format!(
                    "`{:?}` is not a valid rhs for variable assignment",
                    token.content
                ),
            );
            err_collector.print_errs();
            exit(1);
        }
    }
    tree.nodes.len() - 1
}

fn parse_expr(
    tree: &mut AST,
    tokens: &mut TokenStream,
    err_collector: &mut ErrorCollector,
) -> usize {
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
                    let i = parse_single_expr(tree, tokens, err_collector);
                    tree.new_expr(Expression::VarAssign(id, i));
                    return tree.nodes.len() - 1;
                }
                _ => {
                    err_collector.syntax_err(&token, format!("expecting `=` or `(` after {}", id));
                    err_collector.print_errs();
                    exit(1);
                }
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
                err_collector.syntax_err(
                    &token,
                    format!(
                        "expecting an identifier following `let`, found {:?}",
                        token.content
                    ),
                );
                err_collector.print_errs();
                exit(1);
            }
            // get type
            if tokens.next().content == TokenContent::Colon {
                token = tokens.next();
                if let TokenContent::Identifier(ref type_name) = token.content {
                    var_type = DataType::from_str(type_name.clone()).unwrap_or_else(|| {
                        // TODO: type name check should happen in type checks not AST parsing
                        err_collector.syntax_err(
                            &token,
                            format!("`{}` is not a valid data type", type_name),
                        );
                        err_collector.print_errs();
                        exit(1);
                    });
                } else {
                    err_collector.syntax_err(
                        &token,
                        format!("{:?} is not a valid data type", token.content),
                    );
                    err_collector.print_errs();
                    exit(1);
                }
            } else {
                err_collector.syntax_err(
                    &tokens.look_ahead(0),
                    format!(
                        "expecting `:` after variable name, found {:?}",
                        tokens.look_ahead(0).content
                    ),
                );
                err_collector.print_errs();
                exit(1);
            }
            // make sure there is an `=`
            if tokens.next().content != TokenContent::Equal {
                err_collector.syntax_err(
                    &tokens.look_ahead(0),
                    format!(
                        "expecting `=` after variable type, found {:?}",
                        tokens.look_ahead(0).content
                    ),
                );
                err_collector.print_errs();
            }
            // get rhs
            let i = parse_single_expr(tree, tokens, err_collector);
            tree.new_expr(Expression::VarInit(lhs, var_type, i));
            return tree.nodes.len() - 1;
        }
        TokenContent::Return => match tokens.look_ahead(1).content {
            TokenContent::Semicolon => {
                tree.new_expr(Expression::ReturnVoid);
                return tree.nodes.len() - 1;
            }
            _ => {
                let i = parse_single_expr(tree, tokens, err_collector);
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
            err_collector.syntax_err(&token, format!("unidentified token `{:?}`", token.content));
            err_collector.print_errs();
            exit(1);
        }
    };
}

fn parse_fn_def(
    tokens: &mut TokenStream,
    err_collector: &mut ErrorCollector,
) -> Vec<(String, DataType)> {
    let mut token = tokens.next();

    // make sure there is a `(`
    if token.content != TokenContent::RoundParenOpen {
        err_collector.syntax_err(
            &token,
            format!(
                "expecting `(` after function call, found {:?}",
                token.content
            ),
        );
        err_collector.print_errs();
        exit(1);
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
            _ => {
                err_collector.syntax_err(
                    &token,
                    format!(
                        "expecting an identifier as the name of a function arguments, found {:?}",
                        token.content
                    ),
                );
                err_collector.print_errs();
                exit(1);
            }
        }

        token = tokens.next();
        if token.content != TokenContent::Colon {
            err_collector.syntax_err(
                &token,
                format!(
                    "expecting `:` after name of a function arguments, found {:?}",
                    token.content
                ),
            );
            err_collector.print_errs();
            exit(1);
        }

        token = tokens.next();
        if let TokenContent::Identifier(ref type_name) = token.content {
            arg_type = DataType::from_str(type_name.clone()).unwrap_or_else(|| {
                err_collector.syntax_err(&token, format!("{} is not a valid data type", type_name));
                err_collector.print_errs();
                exit(1);
            });
        } else {
            err_collector.syntax_err(
                &token,
                format!(
                    "expecting an identifier after `:` for argument name, found {:?}",
                    token.content
                ),
            );
            err_collector.print_errs();
            exit(1);
        }
        args.push((arg_name, arg_type));

        token = tokens.next();
        match &token.content {
            TokenContent::Comma => {}
            TokenContent::RoundParenClose => break,
            _ => {
                err_collector.syntax_err(
                    &token,
                    format!(
                        "expected `)` or `,` after an argument name, found {:?}",
                        token.content
                    ),
                );
                err_collector.print_errs();
                exit(1);
            }
        }
    }

    args
}

fn parse_top_level(
    tree: &mut AST,
    tokens: &mut TokenStream,
    err_collector: &mut ErrorCollector,
) -> usize {
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
                err_collector.syntax_err(
                    &token,
                    format!(
                        "expected an identifier after `func`, found {:?}",
                        token.content
                    ),
                );
                err_collector.print_errs();
                exit(1);
            }

            let func_args = parse_fn_def(tokens, err_collector);

            token = tokens.next();
            if token.content != TokenContent::BigParenOpen {
                err_collector
                    .syntax_err(&token, format!("expected `{{`, found {:?}", token.content));
                err_collector.print_errs();
                exit(1);
            }
            loop {
                match tokens.look_ahead(1).content {
                    TokenContent::EOF | TokenContent::BigParenClose => {
                        tokens.next();
                        break;
                    }
                    _ => (),
                }
                let i = parse_expr(tree, tokens, err_collector);
                if let Some(node) = tree.nodes.get_mut(i) {
                    if node.is_root {
                        continue;
                    }
                    node.is_root = true;
                    code_block.body.push(i);
                }
            }
            code_block.gen_vars_with_args(&tree.nodes, func_args);
            let expr = Expression::FuncDef(func_name, code_block);
            tree.new_expr(expr);
            tree.nodes.last_mut().unwrap().is_root = true;
        }
        _ => {
            parse_expr(tree, tokens, err_collector);
        }
    }
    tree.nodes.len().saturating_sub(1)
}

pub fn construct_ast(mut tokens: TokenStream, err_collector: &mut ErrorCollector) -> AST {
    let mut ast = AST::default();
    loop {
        parse_top_level(&mut ast, &mut tokens, err_collector);
        if !ast.nodes.is_empty() {
            ast.nodes.last_mut().unwrap().is_top_level = true;
        }
        if tokens.look_ahead(1).content.is_eof() {
            break;
        }
    }
    ast
}
