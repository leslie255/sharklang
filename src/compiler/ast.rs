use std::{
    fmt::Debug,
    rc::{Rc, Weak},
};

use super::{
    error::{CompileError, ErrorCollector, ErrorContent},
    tokens::*,
    typesystem::TypeExpr,
};

#[derive(Clone, Copy, PartialEq)]
pub enum NumValue {
    U(u64),
    I(i64),
    F(f64),
}
impl std::fmt::Debug for NumValue {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U(num) => write!(formatter, "{}u", num)?,
            Self::I(num) => write!(formatter, "{}i", num)?,
            Self::F(num) => write!(formatter, "{}f", num)?,
        };
        Ok(())
    }
}
impl NumValue {
    pub fn into_u64_bytes(&self) -> u64 {
        match self {
            Self::U(u) => u.clone(),
            Self::I(i) => unsafe {
                let ptr = i as *const i64;
                let ptr = ptr as *const u64;
                *ptr
            },
            Self::F(f) => unsafe {
                let ptr = f as *const f64;
                let ptr = ptr as *const u64;
                *ptr
            },
        }
    }
}

impl std::fmt::Display for NumValue {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U(num) => write!(formatter, "{}u", num)?,
            Self::I(num) => write!(formatter, "{}i", num)?,
            Self::F(num) => write!(formatter, "{}f", num)?,
        };
        Ok(())
    }
}

#[allow(dead_code)]
#[derive(Clone)]
pub enum Expression {
    Identifier(Rc<String>),
    NumLiteral(NumValue),
    StrLiteral(usize),
    CharLiteral(u8),

    Def {
        name: Rc<String>,
        dtype: Option<TypeExpr>,
        rhs: Option<Weak<ASTNode>>,
    }, // Name, Type, RHS
    Assign {
        lhs: Weak<ASTNode>,
        rhs: Weak<ASTNode>,
    }, // LHS, RHS
    FnCall {
        name: Rc<String>,
        args: Vec<Weak<ASTNode>>,
    },

    Deref(Weak<ASTNode>),
    TakeAddr(Weak<ASTNode>),

    DataType(TypeExpr),
    TypeCast(Weak<ASTNode>, TypeExpr),
    TypeDef(Rc<String>, TypeExpr),

    Block(Vec<Weak<ASTNode>>),
    Loop(Weak<ASTNode>),

    Return(Option<Weak<ASTNode>>),
    UnsafeReturn,
    Break,
    Continue,

    Extern(Rc<String>, TypeExpr),

    RawASM(Rc<String>),
}

impl Expression {
    pub fn as_block(&self) -> Option<&Vec<Weak<ASTNode>>> {
        if let Self::Block(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn as_fn_def(
        &self,
    ) -> Option<(
        &Rc<String>,                  // name
        &Vec<(Rc<String>, TypeExpr)>, // args
        &TypeExpr,                    // return type
        &Vec<Weak<ASTNode>>,          // body
        bool,                         // is variadic?
    )> {
        if let Self::Def { name, dtype, rhs } = self {
            if let Some((args, ret_type, is_variadic)) = dtype.as_ref()?.as_block() {
                return Some((
                    name,
                    args,
                    ret_type,
                    unsafe { rhs.as_ref()?.as_ptr().as_ref()? }
                        .expr
                        .as_block()?,
                    is_variadic,
                ));
            }
        }
        None
    }

    pub fn as_identifier(&self) -> Option<&Rc<String>> {
        if let Self::Identifier(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_deref(&self) -> Option<&Weak<ASTNode>> {
        if let Self::Deref(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_extern(&self) -> Option<(&Rc<String>, &TypeExpr)> {
        if let Self::Extern(a, b) = self {
            Some((a, b))
        } else {
            None
        }
    }
}

impl Debug for Expression {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(arg0) => formatter.debug_tuple("Identifier").field(arg0).finish(),
            Self::NumLiteral(arg0) => formatter.debug_tuple("NumLiteral").field(arg0).finish(),
            Self::StrLiteral(arg0) => formatter.debug_tuple("StrLiteral").field(arg0).finish(),
            Self::CharLiteral(arg0) => formatter.debug_tuple("CharLiteral").field(arg0).finish(),
            Self::Def { name, dtype, rhs } => {
                if let Some(x) = rhs {
                    formatter
                        .debug_tuple("Def")
                        .field(name)
                        .field(dtype)
                        .field(&x.upgrade().unwrap().expr)
                        .finish()
                } else {
                    formatter
                        .debug_tuple("Def")
                        .field(name)
                        .field(dtype)
                        .finish()
                }
            }
            Self::Assign {
                lhs: arg0,
                rhs: arg1,
            } => formatter
                .debug_struct("Assign")
                .field("lhs", &arg0.upgrade().unwrap().expr)
                .field("rhs", &arg1.upgrade().unwrap().expr)
                .finish(),
            Self::FnCall {
                name: arg0,
                args: arg1,
            } => formatter
                .debug_tuple("FnCall")
                .field(arg0)
                .field(
                    &arg1
                        .iter()
                        .filter_map(|w| w.upgrade())
                        .map(|n| n.expr.clone())
                        .collect::<Vec<Expression>>(),
                )
                .finish(),
            Self::Deref(arg0) => formatter
                .debug_tuple("Deref")
                .field(&arg0.upgrade().unwrap().expr)
                .finish(),
            Self::TakeAddr(arg0) => formatter
                .debug_tuple("TakeAddr")
                .field(&arg0.upgrade().unwrap().expr)
                .finish(),
            Self::RawASM(arg0) => formatter.debug_tuple("RawASM").field(arg0).finish(),
            Self::DataType(arg0) => formatter.debug_tuple("DataType").field(arg0).finish(),
            Self::TypeCast(arg0, arg1) => formatter
                .debug_tuple("TypeCast")
                .field(&arg0.upgrade().unwrap().expr)
                .field(&arg1)
                .finish(),
            Self::Block(arg0) => formatter
                .debug_set()
                .entries(
                    arg0.iter()
                        .filter_map(|w| w.upgrade())
                        .map(|n| n.expr.clone()),
                )
                .finish(),
            Self::Loop(arg0) => formatter
                .debug_tuple("Loop")
                .field(&arg0.upgrade().unwrap().expr)
                .finish(),
            Self::Return(arg0) => {
                if let Some(arg0) = arg0 {
                    formatter
                        .debug_tuple("Return")
                        .field(&arg0.upgrade().unwrap().expr)
                        .finish()
                } else {
                    formatter.debug_tuple("Return").field(arg0).finish()
                }
            }
            Self::UnsafeReturn => formatter.debug_tuple("UnsafeReturn").finish(),
            Self::Break => formatter.debug_tuple("Break").finish(),
            Self::Continue => formatter.debug_tuple("Continue").finish(),
            Self::Extern(arg0, arg1) => formatter
                .debug_tuple("Extern")
                .field(arg0)
                .field(arg1)
                .finish(),
            Self::TypeDef(arg0, arg1) => formatter
                .debug_tuple("TypeDef")
                .field(arg0)
                .field(arg1)
                .finish(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub pos: usize,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct AST {
    node_pool: Vec<Rc<ASTNode>>,
    pub root_nodes: Vec<Weak<ASTNode>>,
    pub strliteral_pool: Vec<String>,
}

impl AST {
    pub fn empty() -> AST {
        AST {
            node_pool: Vec::new(),
            root_nodes: Vec::new(),
            strliteral_pool: Vec::new(),
        }
    }
    pub fn borrow_last_node(&self) -> Weak<ASTNode> {
        if let Some(n) = self.node_pool.last() {
            Rc::downgrade(n)
        } else {
            Weak::new()
        }
    }
    pub fn add_to_node_pool(&mut self, node: ASTNode) -> Weak<ASTNode> {
        self.node_pool.push(Rc::new(node));
        self.borrow_last_node()
    }
}

pub fn parse_tokens_into_ast(
    token_stream: &mut TokenStream,
    err_collector: &mut ErrorCollector,
) -> AST {
    let mut ast = AST::empty();
    loop {
        if token_stream.next().content.is_eof() {
            break;
        }
        match parse_expressions(&mut ast, token_stream, true) {
            Ok(n) => {
                let node = ast.add_to_node_pool(n);
                ast.root_nodes.push(node);
            }
            Err(e) => {
                err_collector.errors.push(e);
                break;
            }
        }
    }
    ast
}

fn parse_expressions(
    ast: &mut AST,
    token_stream: &mut TokenStream,
    expects_semicolon: bool,
) -> Result<ASTNode, CompileError> {
    let current_token = token_stream.current();
    let node: ASTNode;
    match &current_token.content {
        TokenContent::Extern => {
            let pos = current_token.position;
            let name = Rc::clone(token_stream.next().expects_identifier()?);
            if token_stream.next().content != TokenContent::Colon {
                return Err(CompileError::unexpected_token(
                    TokenContent::Colon,
                    token_stream.current(),
                ));
            }
            token_stream.next();
            let dtype = TypeExpr::parse_from_tokens(token_stream)?;
            node = ASTNode {
                pos,
                expr: Expression::Extern(name, dtype),
            }
        }
        TokenContent::Number(num_str) => {
            node = ASTNode {
                pos: current_token.position,
                expr: Expression::NumLiteral(parse_numval(&num_str, current_token)?),
            };
        }
        TokenContent::String(str) => {
            node = ASTNode {
                pos: current_token.position,
                expr: Expression::StrLiteral({
                    ast.strliteral_pool.push(parse_str_content(&str));
                    ast.strliteral_pool.len() - 1
                }),
            };
        }
        TokenContent::Char(val) => {
            node = ASTNode {
                pos: current_token.position,
                expr: Expression::CharLiteral(*val),
            };
        }
        TokenContent::Identifier(_) => {
            node = parse_identifier(ast, token_stream)?;
        }
        TokenContent::Star => {
            let pos = current_token.position;
            token_stream.next();
            let n = parse_expressions(ast, token_stream, false)?;
            node = ASTNode {
                pos,
                expr: Expression::Deref(ast.add_to_node_pool(n)),
            }
        }
        TokenContent::And => {
            let pos = current_token.position;
            token_stream.next();
            let n = parse_expressions(ast, token_stream, false)?;
            node = ASTNode {
                pos,
                expr: Expression::TakeAddr(ast.add_to_node_pool(n)),
            };
        }
        TokenContent::BigParenOpen => {
            let pos = current_token.position;
            let mut body = Vec::<Weak<ASTNode>>::new();
            while token_stream.next().content != TokenContent::BigParenClose {
                let n = parse_expressions(ast, token_stream, true)?;
                body.push(ast.add_to_node_pool(n));
            }
            node = ASTNode {
                pos,
                expr: Expression::Block(body),
            };
        }
        TokenContent::Return => {
            let pos = current_token.position;
            let peek = token_stream.peek(1);
            if peek.indicates_end_of_expr() {
                node = ASTNode {
                    pos,
                    expr: Expression::Return(None),
                };
            } else if peek.content == TokenContent::Underscore {
                token_stream.next();
                node = ASTNode {
                    pos,
                    expr: Expression::UnsafeReturn,
                };
            } else {
                token_stream.next();
                let n = parse_expressions(ast, token_stream, false)?;
                node = ASTNode {
                    pos,
                    expr: Expression::Return(Some(ast.add_to_node_pool(n))),
                };
            }
        }
        TokenContent::Continue => {
            node = ASTNode {
                pos: current_token.position,
                expr: Expression::Continue,
            };
        }
        TokenContent::Break => {
            node = ASTNode {
                pos: current_token.position,
                expr: Expression::Break,
            }
        }
        TokenContent::Typedef => {
            let pos = current_token.position;
            let type_name = Rc::clone(token_stream.next().expects_identifier()?);
            token_stream.next().expects_content(TokenContent::Equal)?;
            token_stream.next();
            let rhs_type = TypeExpr::parse_from_tokens(token_stream)?;
            node = ASTNode {
                pos,
                expr: Expression::TypeDef(type_name, rhs_type),
            }
        }
        TokenContent::Loop => {
            let pos = current_token.position;
            token_stream.next();
            let n = parse_expressions(ast, token_stream, true)?;
            node = ASTNode {
                pos,
                expr: Expression::Loop(ast.add_to_node_pool(n)),
            };
        }
        TokenContent::If => todo!(),
        TokenContent::RawASM(asm_code) => {
            node = ASTNode {
                pos: current_token.position,
                expr: Expression::RawASM(Rc::clone(asm_code)),
            };
        }
        TokenContent::RoundParenOpen => {
            token_stream.next();
            node = parse_expressions(ast, token_stream, false)?;
            token_stream
                .next()
                .expects_content(TokenContent::RoundParenClose)?;
        }
        _ => return Err(CompileError::unexpected_token0(token_stream.current())),
    }

    // detect type casting
    let node = if token_stream.peek(1).content == TokenContent::Squiggle {
        let pos = token_stream.next().position;
        let n = ast.add_to_node_pool(node);
        token_stream.next();
        ASTNode {
            pos,
            expr: Expression::TypeCast(n, TypeExpr::parse_from_tokens(token_stream)?),
        }
    } else {
        node
    };

    if expects_semicolon {
        let current_token = &token_stream.current().content;
        let ended_with_block = current_token.is_big_paren_close();
        let ended_with_asm = if let TokenContent::RawASM(_) = current_token {
            true
        } else {
            false
        };
        let has_semicolon = token_stream.peek(1).content == TokenContent::Semicolon;
        if !has_semicolon && !ended_with_block && !ended_with_asm {
            return Err(CompileError::unexpected_token(
                TokenContent::Semicolon,
                token_stream.current(),
            ));
        }
        if has_semicolon {
            token_stream.next();
        }
    }
    Ok(node)
}

fn parse_identifier(
    ast: &mut AST,
    token_stream: &mut TokenStream,
) -> Result<ASTNode, CompileError> {
    let current_token = token_stream.current();
    match token_stream.peek(1).content {
        TokenContent::Colon => {
            let pos = current_token.position;
            let name = Rc::clone(current_token.expects_identifier()?);
            token_stream.next();
            let mut type_expr = Option::<TypeExpr>::None;
            if token_stream.peek(1).content != TokenContent::Equal {
                token_stream.next();
                // the token after colon
                // if it's not equal sign means it has an explicit type expression
                type_expr = Some(TypeExpr::parse_from_tokens(token_stream)?);
            }
            let peek = token_stream.peek(1);
            let rhs = if peek.content == TokenContent::Equal {
                token_stream.next();
                token_stream.next();
                let n = parse_expressions(ast, token_stream, false)?;
                Some(ast.add_to_node_pool(n))
            } else if peek.indicates_end_of_expr() {
                None
            } else {
                return Err(CompileError::unexpected_token_multiple(
                    vec![TokenContent::Equal, TokenContent::Semicolon],
                    &peek,
                ));
            };
            if type_expr.is_none() && rhs.is_none() {
                return Err(CompileError {
                    content: ErrorContent::UnableToInferType {
                        var_name: Rc::clone(&name),
                    },
                    position: token_stream.current().position,
                    length: token_stream.current().len,
                });
            }
            Ok(ASTNode {
                pos,
                expr: Expression::Def {
                    name,
                    dtype: type_expr,
                    rhs,
                },
            })
        }
        TokenContent::Equal => {
            let pos = current_token.position;
            let lhs_node = ASTNode {
                pos,
                expr: Expression::Identifier(Rc::clone(
                    token_stream.current().expects_identifier()?,
                )),
            };
            token_stream.next(); // equal sign
            token_stream.next(); // token after equal sign
            let rhs_node = parse_expressions(ast, token_stream, false)?;
            let assign_expr = Expression::Assign {
                lhs: ast.add_to_node_pool(lhs_node),
                rhs: ast.add_to_node_pool(rhs_node),
            };
            Ok(ASTNode {
                pos,
                expr: assign_expr,
            })
        }
        TokenContent::RoundParenOpen => {
            let pos: usize = current_token.position;
            let fn_name = Rc::clone(current_token.expects_identifier()?);
            let mut fn_args = Vec::<Weak<ASTNode>>::new();
            token_stream.next();
            if token_stream.next().content != TokenContent::RoundParenClose {
                loop {
                    let n = parse_expressions(ast, token_stream, false)?;
                    fn_args.push(ast.add_to_node_pool(n));
                    let token = token_stream.next();
                    if token.content == TokenContent::RoundParenClose {
                        break;
                    } else if token.content == TokenContent::Comma {
                        if token_stream.peek(1).content == TokenContent::RoundParenClose {
                            token_stream.next();
                            break;
                        }
                        token_stream.next();
                    } else {
                        return Err(CompileError::unexpected_token_multiple(
                            vec![TokenContent::RoundParenClose, TokenContent::Comma],
                            token_stream.current(),
                        ));
                    }
                }
            }
            Ok(ASTNode {
                pos,
                expr: Expression::FnCall {
                    name: fn_name,
                    args: fn_args,
                },
            })
        }
        _ => Ok(ASTNode {
            pos: current_token.position,
            expr: Expression::Identifier(Rc::clone(current_token.expects_identifier()?)),
        }),
    }
}

fn parse_str_content(src: &String) -> String {
    let mut new_str = String::new();
    let mut iter = src.chars();
    loop {
        let mut ch = if let Some(c) = iter.next() {
            c
        } else {
            break;
        };
        if ch == '\\' {
            ch = if let Some(c) = iter.next() {
                c
            } else {
                break;
            };
            match ch {
                'n' => new_str.push('\n'),
                '\\' => new_str.push('\\'),
                '0' => new_str.push('\0'),
                't' => new_str.push('\t'),
                _ => new_str.push(ch),
            }
        } else {
            new_str.push(ch);
        }
    }
    new_str
}

fn parse_numval(src: &String, current_token: &Token) -> Result<NumValue, CompileError> {
    if src.contains('.') {
        if let Ok(f) = src.parse::<f64>() {
            return Ok(NumValue::F(f));
        }
    } else if src.chars().next() == Some('-') {
        if let Ok(i) = src.parse::<i64>() {
            return Ok(NumValue::I(i));
        }
    } else {
        if let Ok(u) = src.parse::<u64>() {
            return Ok(NumValue::U(u));
        }
    }
    Err(CompileError {
        content: ErrorContent::InvalidNumberFormat(src.to_string()),
        position: current_token.position,
        length: current_token.len,
    })
}
