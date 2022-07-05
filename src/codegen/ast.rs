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
#[derive(Debug, Clone, PartialEq)]
pub struct CodeBlock {
    pub body: Vec<usize>,
    pub vars: HashMap<String, VarInfo>,
    pub stack_depth: u64,
    pub arg_types: Vec<DataType>,
    pub has_vars: bool,
    pub return_type: DataType,
    pub parent: usize,
}
impl Default for CodeBlock {
    fn default() -> Self {
        CodeBlock {
            body: Vec::default(),
            vars: HashMap::default(),
            stack_depth: u64::default(),
            arg_types: Vec::default(),
            has_vars: bool::default(),
            return_type: DataType::Void,
            parent: usize::MAX,
        }
    }
}
impl CodeBlock {
    pub fn var_type(&self, var_name: &String, ast: &AST) -> DataType {
        match self.vars.get(var_name) {
            Some(x) => x.data_type.clone(),
            None => {
                if self.parent == usize::MAX {
                    panic!();
                }
                let parent_block = if let Expression::Block(b) = ast.expr(self.parent) {
                    b
                } else {
                    panic!();
                };
                parent_block.var_type(var_name, ast)
            }
        }
    }
    pub fn var_addr(&self, var_name: &String, ast: &AST) -> Option<u64> {
        match self.vars.get(var_name) {
            Some(x) => Some(x.addr),
            None => {
                let parent_block = if let Expression::Block(b) = ast.expr(self.parent) {
                    b
                } else {
                    return None;
                };
                parent_block.var_addr(var_name, ast)
            }
        }
    }
    pub fn var_info<'a>(&'a self, var_name: &String, ast: &'a AST) -> Option<&'a VarInfo> {
        match self.vars.get(var_name) {
            Some(x) => Some(x),
            None => {
                if self.parent == usize::MAX {
                    return None;
                }
                let parent_block = if let Expression::Block(b) = ast.expr(self.parent) {
                    b
                } else {
                    return None;
                };
                parent_block.var_info(var_name, ast)
            }
        }
    }
    pub fn gen_vars_with_args(&mut self, nodes: &Vec<ASTNode>, args: Vec<(String, DataType)>) {
        // this should technically be in the codegen part but i have no idea how to move it there
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
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // Non-Recursive expression
    Identifier(String),
    NumberLiteral(u64),
    StringLiteral(String),
    CharLiteral(u8),
    // Recursive expression
    FuncCall(String, Vec<usize>),     // function name, arguments
    VarInit(String, DataType, usize), // lhs, rhs
    VarAssign(String, usize),         // lhs, rhs
    TypeCast(usize, DataType),

    // Raw ASM
    Label(String),
    RawASM(String),

    // Control flows
    Block(CodeBlock),
    FuncDef(String, usize), // name, code block
    Loop(usize),            // code block
    ReturnVoid,
    ReturnVal(usize),
    UnsafeReturn,

    Unknown,
}
impl Default for Expression {
    fn default() -> Self {
        return Expression::Unknown;
    }
}
impl Expression {
    pub fn description(&self) -> String {
        match self {
            Expression::Identifier(name) => format!("`{}`", name),
            Expression::NumberLiteral(num) => format!("`{}`", num),
            Expression::StringLiteral(str) => format!("{:?}", str),
            Expression::CharLiteral(char) => format!("{:?}", char),
            Expression::FuncCall(fn_name, _) => format!("{}(...)", fn_name),
            Expression::VarInit(_, _, _) => String::from("variable declaration"),
            Expression::VarAssign(_, _) => String::from("variable assign"),
            Expression::TypeCast(_, _) => String::from("type cast"),
            Expression::Label(name) => format!("{}:", name),
            Expression::RawASM(asm) => format!("`{}`", asm.trim().clone()),
            Expression::Block(_) => String::from("block"),
            Expression::FuncDef(fn_name, _) => format!("func `{}`(...)`", fn_name),
            Expression::Loop(_) => format!("loop {{...}}"),
            Expression::ReturnVoid => String::from("return"),
            Expression::ReturnVal(_) => String::from("return ..."),
            Expression::UnsafeReturn => String::from("_return"),
            Expression::Unknown => String::from("UNKNWON"),
        }
    }
}
#[derive(Default, Clone)]
pub struct ASTNode {
    pub expr: Expression,
    pub is_root: bool,
    pub is_top_level: bool,
    pub position: usize,
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
    // get expression but if the expression is a typecast, unwrap it
    #[allow(unused)]
    pub fn expr_no_typecast(&self, i: usize) -> &Expression {
        let expr = unsafe { &self.nodes.get_unchecked(i).expr };
        if let Expression::TypeCast(unwrapped_i, _) = expr {
            unsafe { &self.nodes.get_unchecked(*unwrapped_i).expr }
        } else {
            expr
        }
    }
    // get node but if the expression is a typecast, unwrap it
    pub fn node_no_typecast(&self, i: usize) -> &ASTNode {
        let node = unsafe { self.nodes.get_unchecked(i) };
        if let Expression::TypeCast(unwrapped_i, _) = &node.expr {
            unsafe { &self.nodes.get_unchecked(*unwrapped_i) }
        } else {
            node
        }
    }
    pub fn new_expr(&mut self, expr: Expression, position: usize) {
        match expr {
            Expression::FuncDef(ref func_name, _) => {
                self.func_defs.insert(func_name.clone(), self.nodes.len());
            }
            _ => (),
        }
        self.nodes.push(ASTNode {
            expr,
            is_root: false,
            is_top_level: false,
            position,
        });
    }
    pub fn fn_arg_types(&self, fn_name: &String) -> Option<&Vec<DataType>> {
        if let Expression::FuncDef(_, block_i) = &self.node(*self.func_defs.get(fn_name)?).expr {
            if let Expression::Block(block) = self.expr(*block_i) {
                return Some(&block.arg_types);
            }
        }
        None
    }
    pub fn fn_block(&self, fn_name: &String) -> Option<&CodeBlock> {
        if let Expression::FuncDef(_, block_i) = &self.node(*self.func_defs.get(fn_name)?).expr {
            if let Expression::Block(block) = self.expr(*block_i) {
                return Some(&block);
            }
        }
        None
    }
}

fn parse_fn_call_args(
    tokens: &mut TokenStream,
    tree: &mut AST,
    err_collector: &mut ErrorCollector,
) -> Vec<usize> {
    let mut args: Vec<usize> = Vec::new();
    let mut token = tokens.next();

    // make sure there is a `(`
    if TokenContent::RoundParenOpen != token.content {
        err_collector.syntax_err(&token, "expected a `(` for function call".to_string());
        err_collector.print_errs();
        exit(1);
    }

    // if there is a `)` coming immediately after the `(`, there are no arguments
    match &tokens.look_ahead(1).content {
        TokenContent::RoundParenClose => {
            tokens.next();
            return args;
        }
        _ => (),
    }

    loop {
        // get the next argument
        let i = parse_single_expr(tree, tokens, err_collector);
        args.push(i);
        token = tokens.next();
        match &token.content {
            TokenContent::Comma => {}
            TokenContent::RoundParenClose => break,
            _ => {
                err_collector.syntax_err(
                    &token,
                    format!("expected `)` or `,` after a function argument"),
                );
                err_collector.print_errs();
                exit(1);
            }
        }
    }

    args
}

fn parse_single_expr(
    tree: &mut AST,
    tokens: &mut TokenStream,
    err_collector: &mut ErrorCollector,
) -> usize {
    let mut token = tokens.next();
    match token.content {
        TokenContent::UInt(uint) => {
            tree.new_expr(Expression::NumberLiteral(uint), token.position);
        }
        TokenContent::String(str) => {
            tree.new_expr(Expression::StringLiteral(str), token.position);
        }
        TokenContent::Char(byte) => {
            tree.new_expr(Expression::CharLiteral(byte), token.position);
        }
        TokenContent::Identifier(id) => match tokens.look_ahead(1).content {
            TokenContent::RoundParenOpen => {
                // is a function call
                let args = parse_fn_call_args(tokens, tree, err_collector);
                tree.new_expr(Expression::FuncCall(id, args), token.position);
            }
            _ => {
                // is a variable
                tree.new_expr(Expression::Identifier(id), token.position);
            }
        },
        _ => {
            err_collector.syntax_err(&token, format!("what the fuck is this shit?"));
            err_collector.print_errs();
            exit(1);
        }
    }
    if tokens.look_ahead(1).content == TokenContent::Squiggle {
        // type cast
        tokens.next();
        token = tokens.next();
        if let TokenContent::Identifier(ref id) = token.content {
            let data_type = DataType::from_str(&id).unwrap_or_else(|| {
                err_collector.syntax_err(&token, format!("expecting a valid data type after `~`"));
                err_collector.print_errs();
                exit(1);
            });
            tree.new_expr(
                Expression::TypeCast(tree.nodes.len() - 1, data_type),
                token.position,
            );
        } else {
            err_collector.syntax_err(&token, format!("expecting a data type after `~`"));
            err_collector.print_errs();
            exit(1);
        }
    }
    tree.nodes.len() - 1
}

fn parse_inside_fn(
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
                    let args = parse_fn_call_args(tokens, tree, err_collector);
                    tree.new_expr(Expression::FuncCall(id.clone(), args), token.position);
                    return tree.nodes.len() - 1;
                }
                _ => {}
            }
            token = tokens.next();
            match token.content {
                TokenContent::Colon => {
                    tree.new_expr(Expression::Label(id), token.position);
                    return tree.nodes.len() - 1;
                }
                TokenContent::Equal => {
                    // is variable assign
                    let i = parse_single_expr(tree, tokens, err_collector);
                    tree.new_expr(Expression::VarAssign(id, i), token.position);
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
            // rhs could be: number literal, string literal, identifier, function call
            // get lhs
            token = tokens.next();
            let lhs: String;
            let var_type: DataType;
            if let TokenContent::Identifier(var_name) = token.content {
                lhs = var_name;
            } else {
                err_collector
                    .syntax_err(&token, format!("expecting an identifier following `let`"));
                err_collector.print_errs();
                exit(1);
            }
            // get type
            if tokens.next().content == TokenContent::Colon {
                token = tokens.next();
                if let TokenContent::Identifier(ref type_name) = token.content {
                    var_type = DataType::from_str(type_name).unwrap_or_else(|| {
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
                    format!("expecting `:` after variable name"),
                );
                err_collector.print_errs();
                exit(1);
            }
            // make sure there is an `=`
            if tokens.next().content != TokenContent::Equal {
                err_collector.syntax_err(&token, format!("expecting `=` after variable type",));
                err_collector.print_errs();
            }
            // get rhs
            let i = parse_single_expr(tree, tokens, err_collector);
            tree.new_expr(Expression::VarInit(lhs, var_type, i), token.position);
            return tree.nodes.len() - 1;
        }
        TokenContent::Loop => {
            token = tokens.next();
            if token.content != TokenContent::BigParenOpen {
                err_collector.syntax_err(&token, format!("expecting `{{` after `loop`"));
                err_collector.print_errs();
                exit(1);
            }
            let mut block = CodeBlock::default();
            loop {
                match tokens.look_ahead(1).content {
                    TokenContent::EOF | TokenContent::BigParenClose => {
                        tokens.next();
                        break;
                    }
                    _ => (),
                }
                let i = parse_inside_fn(tree, tokens, err_collector);
                if i != usize::MAX {
                    block.body.push(i);
                }
            }
            tree.new_expr(Expression::Block(block), token.position);
            tree.new_expr(Expression::Loop(tree.nodes.len() - 1), token.position);
            return tree.nodes.len() - 1;
        }
        TokenContent::Return => match tokens.look_ahead(1).content {
            TokenContent::Semicolon => {
                tree.new_expr(Expression::ReturnVoid, token.position);
                return tree.nodes.len() - 1;
            }
            _ => {
                let i = parse_single_expr(tree, tokens, err_collector);
                tree.new_expr(Expression::ReturnVal(i), token.position);
                return tree.nodes.len() - 1;
            }
        },
        TokenContent::UnsafeReturn => {
            tree.new_expr(Expression::UnsafeReturn, token.position);
            return tree.nodes.len() - 1;
        }
        TokenContent::Semicolon => {
            return usize::MAX;
        }
        TokenContent::RawASM(text) => {
            tree.new_expr(Expression::RawASM(text), token.position);
            return tree.nodes.len() - 1;
        }
        TokenContent::String(str) => {
            tree.new_expr(Expression::StringLiteral(str), token.position);
            tree.new_expr(
                Expression::FuncCall(String::from("println"), vec![tree.nodes.len() - 1]),
                token.position,
            );
            return tree.nodes.len() - 1;
        }
        _ => {
            err_collector.syntax_err(&token, format!("unidentified token `{:?}`", token.content));
            err_collector.print_errs();
            exit(1);
        }
    };
}

fn parse_fn_def_args(
    tokens: &mut TokenStream,
    err_collector: &mut ErrorCollector,
) -> Vec<(String, DataType)> {
    let mut token = tokens.next();

    // make sure there is a `(`
    if token.content != TokenContent::RoundParenOpen {
        err_collector.syntax_err(&token, format!("expecting `(` after function call"));
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
                    format!("expecting an identifier as the name of a function arguments"),
                );
                err_collector.print_errs();
                exit(1);
            }
        }

        token = tokens.next();
        if token.content != TokenContent::Colon {
            err_collector.syntax_err(
                &token,
                format!("expecting `:` after name of a function arguments"),
            );
            err_collector.print_errs();
            exit(1);
        }

        token = tokens.next();
        if let TokenContent::Identifier(ref type_name) = token.content {
            arg_type = DataType::from_str(type_name).unwrap_or_else(|| {
                err_collector.syntax_err(&token, format!("{} is not a valid data type", type_name));
                err_collector.print_errs();
                exit(1);
            });
        } else {
            err_collector.syntax_err(
                &token,
                format!("expecting an identifier after `:` for argument name"),
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
                    format!("expected `)` or `,` after an argument name"),
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
                err_collector.syntax_err(&token, format!("expected an identifier after `func`"));
                err_collector.print_errs();
                exit(1);
            }

            let func_args = parse_fn_def_args(tokens, err_collector);

            token = tokens.next();
            // if it's `->` then there is a return type, otherwise it's `void`
            match token.content {
                TokenContent::BigParenOpen => (),
                TokenContent::ReturnArrow => {
                    token = tokens.next();
                    if let TokenContent::Identifier(ref type_name) = token.content {
                        code_block.return_type =
                            DataType::from_str(type_name).unwrap_or_else(|| {
                                err_collector.syntax_err(
                                    &token,
                                    format!("`{}` is not a valid data type", type_name),
                                );
                                err_collector.print_errs();
                                exit(1);
                            });
                    }
                    token = tokens.next();
                    if token.content != TokenContent::BigParenOpen {
                        err_collector.syntax_err(&token, format!("expected `{{` or `->`"));
                        err_collector.print_errs();
                        exit(1);
                    }
                }
                _ => {
                    err_collector.syntax_err(&token, format!("expected `{{` or `->`"));
                    err_collector.print_errs();
                    exit(1);
                }
            }
            loop {
                match tokens.look_ahead(1).content {
                    TokenContent::EOF | TokenContent::BigParenClose => {
                        tokens.next();
                        break;
                    }
                    _ => (),
                }
                let i = parse_inside_fn(tree, tokens, err_collector);
                if let Some(node) = tree.nodes.get_mut(i) {
                    if node.is_root {
                        continue;
                    }
                    node.is_root = true;
                    code_block.body.push(i);
                }
            }
            code_block.gen_vars_with_args(&tree.nodes, func_args);
            let parent = tree.nodes.len();
            for i in &code_block.body {
                match tree.nodes.get_mut(*i).unwrap().expr {
                    Expression::Loop(block_i) => {
                        match &mut tree.nodes.get_mut(block_i).unwrap().expr {
                            Expression::Block(block) => block.parent = parent,
                            _ => panic!(),
                        }
                    }
                    _ => (),
                }
            }
            tree.new_expr(Expression::Block(code_block), token.position);
            tree.new_expr(
                Expression::FuncDef(func_name, tree.nodes.len() - 1),
                token.position,
            );
            tree.nodes.last_mut().unwrap().is_root = true;
        }
        _ => {
            parse_inside_fn(tree, tokens, err_collector);
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
