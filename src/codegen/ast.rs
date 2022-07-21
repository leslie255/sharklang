use super::error::*;
use super::tokens::*;
use super::typecheck::*;

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
    pub parent: usize, // parent block
    pub owner: usize,  // loop, func def, if, etc...
}
impl Default for CodeBlock {
    fn default() -> Self {
        CodeBlock {
            body: Vec::default(),
            vars: HashMap::default(),
            stack_depth: u64::default(),
            arg_types: Vec::default(),
            has_vars: bool::default(),
            return_type: DataType::ToBeDetermined,
            parent: usize::MAX,
            owner: usize::MAX,
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
                let parent_block = if let Expression::Block(b) = &ast.nodes.get(self.parent)?.expr {
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
    pub fn fn_return_type(&self, ast: &AST) -> Option<DataType> {
        if let Expression::FuncDef(_, fn_block_i) = ast.node_no_typecast(self.owner).expr {
            // if is a function
            if let Expression::Block(fn_block) = &ast.node_no_typecast(fn_block_i).expr {
                Some(fn_block.return_type.clone())
            } else {
                None
            }
        } else {
            // if is not a function
            if let Expression::Block(parent_block) = &ast.node_no_typecast(self.parent).expr {
                parent_block.fn_return_type(ast)
            } else {
                None
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
    GetAddress(usize),
    Dereference(usize),

    // Raw ASM
    Label(String),
    RawASM(String),

    // Control flows
    Block(CodeBlock),
    FuncDef(String, usize),              // name, code block
    Loop(usize),                         // code block
    If(usize, usize, Vec<usize>, usize), // condition, if block, else if blocks, else block
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
    pub fn has_block(&self) -> bool {
        match self {
            Expression::If(..) | Expression::Loop(..) => true,
            _ => false,
        }
    }
    pub fn get_block_unchecked(&self) -> &CodeBlock {
        match self {
            Expression::Block(block) => {
                return block;
            }
            _ => panic!(),
        }
    }
    pub fn get_block(&self) -> Option<&CodeBlock> {
        match self {
            Expression::Block(block) => {
                return Some(block);
            }
            _ => None
        }
    }
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
            Expression::GetAddress(_) => String::from("get address"),
            Expression::Dereference(_) => String::from("dereference"),
            Expression::Label(name) => format!("{}:", name),
            Expression::RawASM(asm) => format!("`{}`", asm.trim().clone()),
            Expression::Block(_) => String::from("block"),
            Expression::FuncDef(fn_name, _) => format!("func `{}(...)`", fn_name),
            Expression::Loop(_) => format!("loop {{...}}"),
            Expression::If(_, _, _, _) => format!("if {{...}}"),
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
    pub fn node_mut(&mut self, i: usize) -> &mut ASTNode {
        unsafe { self.nodes.get_unchecked_mut(i) }
    }
    // get expression but if the expression is a typecast, unwrap it
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

#[allow(unused_assignments)]
fn recursive_parse_exprs(
    tokens: &mut TokenStream,
    target: &mut AST,
    err_collector: &mut ErrorCollector,
) -> Option<usize> {
    let mut token: Token;
    macro_rules! recursive_call {
        () => {{
            let i = recursive_parse_exprs(tokens, target, err_collector);
            if tokens.look_ahead(1).content == TokenContent::EOF {
                return None;
            }
            token = tokens.current();
            i
        }};
    }
    macro_rules! token_get_id {
        ($t: expr) => {
            if let TokenContent::Identifier(ref x) = $t.content {
                x.clone()
            } else {
                err_collector.add_err(
                    ErrorType::Syntax,
                    $t.position,
                    $t.len,
                    format!("expecting an identifier"),
                );
                tokens.skip_to_next_expr();
                return None;
            }
        };
    }
    macro_rules! token_extract_data_type {
        ($t: expr) => {
            if let Some(t) = DataType::from_str(&token_get_id!($t)) {
                t
            } else {
                err_collector.add_err(
                    ErrorType::Syntax,
                    $t.position,
                    $t.len,
                    format!("expecting a type name"),
                );
                tokens.skip_to_next_expr();
                return None;
            }
        };
    }
    macro_rules! token_ensure_type {
        ($token: expr, $token_type: expr) => {
            if $token_type != $token.content {
                err_collector.add_err(
                    ErrorType::Syntax,
                    $token.position,
                    $token.len,
                    format!("expecting a `{:?}`", $token_type),
                );
                tokens.skip_to_next_expr();
                return None;
            }
        };
        ($token: expr, $type0: expr, $type1: expr) => {
            if $type0 != $token.content && $type1 != $token.content {
                err_collector.add_err(
                    ErrorType::Syntax,
                    $token.position,
                    $token.len,
                    format!("expecting `{:?}` or `{:?}`", $type0, $type1),
                );
                tokens.skip_to_next_expr();
                return None;
            }
        };
    }
    loop {
        token = tokens.next();
        match &token.content {
            TokenContent::RawASM(code) => {
                target.new_expr(Expression::RawASM(code.clone()), token.position);
                break;
            }
            TokenContent::UInt(num) => {
                target.new_expr(Expression::NumberLiteral(*num), token.position);
                break;
            }
            TokenContent::Char(ch) => {
                target.new_expr(Expression::CharLiteral(*ch as u8), token.position);
                break;
            }
            TokenContent::String(str) => {
                target.new_expr(Expression::StringLiteral(str.clone()), token.position);
                break;
            }
            TokenContent::And => {
                let position = token.position;
                if let Some(i) = recursive_call!() {
                    target.new_expr(Expression::GetAddress(i), position);
                    break;
                } else {
                    err_collector.add_err(
                        ErrorType::Syntax,
                        position,
                        1,
                        format!("Expected an expression"),
                    );
                    return None;
                }
            }
            TokenContent::Dollar => {
                let position = token.position;
                if let Some(i) = recursive_call!() {
                    target.new_expr(Expression::Dereference(i), position);
                    break;
                } else {
                    err_collector.add_err(
                        ErrorType::Syntax,
                        position,
                        1,
                        format!("Expected an expression"),
                    );
                    return None;
                }
            }
            TokenContent::Identifier(id) => match tokens.look_ahead(1).content {
                TokenContent::Equal => {
                    // variable assign
                    let var_name = id.clone();
                    tokens.next();
                    recursive_call!();
                    target.new_expr(
                        Expression::VarAssign(var_name, target.nodes.len() - 1),
                        token.position,
                    );
                    break;
                }
                TokenContent::RoundParenOpen => {
                    // function call
                    let func_name = id.clone();
                    let position = token.position;
                    let mut args: Vec<usize> = Vec::new();
                    token_ensure_type!(tokens.next(), TokenContent::RoundParenOpen);
                    if tokens.look_ahead(1).content == TokenContent::RoundParenClose {
                        // there are no arguments
                        target.new_expr(Expression::FuncCall(func_name, args), position);
                        break;
                    }
                    loop {
                        if let Some(i) = recursive_call!() {
                            args.push(i);
                        }
                        token = tokens.next();
                        // comma means there is another argument, closing round paren means end
                        token_ensure_type!(
                            token,
                            TokenContent::Comma,
                            TokenContent::RoundParenClose
                        );
                        match token.content {
                            TokenContent::RoundParenClose => break,
                            _ => (),
                        }
                    }
                    target.new_expr(Expression::FuncCall(func_name, args), position);
                    break;
                }
                TokenContent::Colon => {
                    // Label
                    let label_name = id.clone();
                    let position = token.position;
                    tokens.next();
                    target.new_expr(Expression::Label(label_name), position);
                    break;
                }
                _ => {
                    target.new_expr(Expression::Identifier(id.clone()), token.position);
                    break;
                }
            },
            TokenContent::Return => {
                let look_ahead = tokens.look_ahead(1);
                if look_ahead.indicates_end_of_expr() {
                    target.new_expr(Expression::ReturnVoid, token.position);
                    break;
                } else if look_ahead.content == TokenContent::Underscore {
                    tokens.next();
                    target.new_expr(Expression::UnsafeReturn, token.position);
                    break;
                } else {
                    recursive_call!();
                    target.new_expr(
                        Expression::ReturnVal(target.nodes.len() - 1),
                        token.position,
                    );
                    break;
                }
            }
            TokenContent::Let => {
                let var_name = token_get_id!(tokens.next());
                let mut var_type = DataType::ToBeDetermined;
                token = tokens.next();
                token_ensure_type!(token, TokenContent::Colon, TokenContent::Equal);
                match token.content {
                    TokenContent::Colon => {
                        var_type = token_extract_data_type!(tokens.next());
                        token = tokens.next();
                        if token.content != TokenContent::Equal {}
                    }
                    TokenContent::Equal => (),
                    _ => (),
                }
                recursive_call!();
                target.new_expr(
                    Expression::VarInit(var_name.clone(), var_type, target.nodes.len() - 1),
                    token.position,
                );
                break;
            }
            TokenContent::Func => {
                let fn_def_pos = token.position;
                token = tokens.next();

                // function name and arguments
                let fn_name = token_get_id!(token);
                let mut fn_args: Vec<(String, DataType)> = Vec::new();
                let mut code_block = CodeBlock::default();
                token_ensure_type!(tokens.next(), TokenContent::RoundParenOpen);
                if tokens.look_ahead(1).content != TokenContent::RoundParenClose {
                    // has ast least one arguments
                    loop {
                        token = tokens.next();
                        let arg_name = token_get_id!(token);

                        token = tokens.next();
                        token_ensure_type!(token, TokenContent::Colon);

                        token = tokens.next();
                        let arg_type = token_extract_data_type!(token);

                        fn_args.push((arg_name, arg_type));

                        // comma means there is another argument, closing round paren means end
                        token = tokens.next();
                        token_ensure_type!(
                            token,
                            TokenContent::Comma,
                            TokenContent::RoundParenClose
                        );
                        match token.content {
                            TokenContent::RoundParenClose => break,
                            _ => (),
                        }
                    }
                } else {
                    // no arguments
                    tokens.next();
                }

                // function body
                token = tokens.next();
                token_ensure_type!(token, TokenContent::BigParenOpen, TokenContent::ReturnArrow);
                if token.content == TokenContent::ReturnArrow {
                    code_block.return_type = token_extract_data_type!(tokens.next());
                    token_ensure_type!(tokens.next(), TokenContent::BigParenOpen);
                }
                loop {
                    if let Some(i) = recursive_call!() {
                        target.nodes.get_mut(i).unwrap().is_root = true;
                        code_block.body.push(target.nodes.len() - 1);
                    }
                    if tokens.look_ahead(1).content == TokenContent::BigParenClose {
                        tokens.next();
                        break;
                    }
                }
                code_block.gen_vars_with_args(&target.nodes, fn_args);

                // set owners and parents for sub blocks
                let parent = target.nodes.len();
                let a: Vec<(usize, Expression)> = code_block
                    .body
                    .iter()
                    .map(|x| (*x, target.expr(*x)))
                    .filter(|(_, expr)| expr.has_block())
                    .map(|(i, expr)| (i, expr.clone()))
                    .collect();
                let mut ast_changes: Vec<(usize, Expression)> = Vec::new();
                a.iter().for_each(|(ast_i, expr)| match expr {
                    Expression::Loop(loop_block_i) => {
                        let mut new_block = target.expr(*loop_block_i).get_block_unchecked().clone();
                        new_block.parent = parent;
                        new_block.owner = *ast_i;
                        ast_changes.push((*loop_block_i, Expression::Block(new_block)));
                    }
                    Expression::If(_, if_block_i, elif_blocks, else_block_i) => {
                        let mut new_block = target.expr(*if_block_i).get_block_unchecked().clone();
                        new_block.parent = parent;
                        new_block.owner = *ast_i;
                        ast_changes.push((*if_block_i, Expression::Block(new_block)));
                        for elif_block_i in elif_blocks {
                            let mut new_block = target.expr(*elif_block_i).get_block_unchecked().clone();
                            new_block.parent = parent;
                            new_block.owner = *ast_i;
                            ast_changes.push((*elif_block_i, Expression::Block(new_block)));
                        }
                        if *else_block_i == usize::MAX {
                            return;
                        }
                        let mut new_block = target.expr(*else_block_i).get_block_unchecked().clone();
                        new_block.parent = parent;
                        new_block.owner = *ast_i;
                        ast_changes.push((*else_block_i, Expression::Block(new_block)));
                    }
                    _ => (),
                });
                for (i, expr) in ast_changes {
                    target.node_mut(i).expr = expr;
                }

                code_block.owner = target.nodes.len() + 1;
                target.new_expr(Expression::Block(code_block), fn_def_pos);
                target.new_expr(
                    Expression::FuncDef(fn_name, target.nodes.len() - 1),
                    fn_def_pos,
                );
                break;
            }
            TokenContent::Loop => {
                let loop_pos = token.position;
                token_ensure_type!(tokens.next(), TokenContent::BigParenOpen);
                let mut code_block = CodeBlock::default();
                loop {
                    if let Some(i) = recursive_call!() {
                        code_block.body.push(i);
                    }
                    if tokens.look_ahead(1).content == TokenContent::BigParenClose {
                        tokens.next();
                        break;
                    }
                }
                code_block.owner = target.nodes.len() + 1;
                target.new_expr(Expression::Block(code_block), loop_pos);
                target.new_expr(Expression::Loop(target.nodes.len() - 1), loop_pos);
                break;
            }
            TokenContent::If => {
                let if_pos = token.position;
                let mut if_block = CodeBlock::default();
                let mut else_block: Option<CodeBlock> = None;
                let condition = if let Some(i) = recursive_call!() {
                    i
                } else {
                    usize::MAX
                };
                token_ensure_type!(tokens.next(), TokenContent::BigParenOpen);
                loop {
                    if let Some(i) = recursive_call!() {
                        if_block.body.push(i);
                    }
                    if tokens.look_ahead(1).content == TokenContent::BigParenClose {
                        tokens.next();
                        break;
                    }
                }
                if tokens.look_ahead(1).content == TokenContent::Else {
                    tokens.next();
                    token_ensure_type!(tokens.next(), TokenContent::BigParenOpen);
                    let mut else_block_unassigned = CodeBlock::default();
                    loop {
                        if let Some(i) = recursive_call!() {
                            else_block_unassigned.body.push(i);
                        }
                        if tokens.look_ahead(1).content == TokenContent::BigParenClose {
                            tokens.next();
                            break;
                        }
                    }
                    else_block = Some(else_block_unassigned);
                }
                target.new_expr(Expression::Block(if_block), if_pos);
                let if_block_i = target.nodes.len() - 1;
                let else_block_i: usize;
                if let Some(b) = else_block {
                    target.new_expr(Expression::Block(b), if_pos);
                    else_block_i = target.nodes.len() - 1;
                } else {
                    else_block_i = usize::MAX;
                }
                target.new_expr(
                    Expression::If(condition, if_block_i, Vec::new(), else_block_i),
                    if_pos,
                );
                break;
            }
            TokenContent::EOF => return None,
            _ => {
                if !token.indicates_end_of_expr() {
                    err_collector.add_err(
                        ErrorType::Syntax,
                        token.position,
                        token.len,
                        format!("what is this?"),
                    );
                    tokens.skip_to_next_expr();
                }
                return None;
            }
        }
    }
    if tokens.look_ahead(1).content == TokenContent::Squiggle {
        // type cast
        let position = tokens.next().position;
        let casted_type = token_extract_data_type!(tokens.next());
        target.new_expr(
            Expression::TypeCast(target.nodes.len() - 1, casted_type),
            position,
        );
    }
    return Some(target.nodes.len() - 1);
}

pub fn construct_ast(mut tokens: TokenStream, err_collector: &mut ErrorCollector) -> AST {
    let mut ast = AST::default();
    loop {
        if let Some(i) = recursive_parse_exprs(&mut tokens, &mut ast, err_collector) {
            ast.nodes.get_mut(i).unwrap().is_top_level = true;
        }
        if tokens.look_ahead(1).content.is_eof() {
            break;
        }
    }
    ast
}
