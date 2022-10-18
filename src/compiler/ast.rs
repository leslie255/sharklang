use std::{
    fmt::{Debug, Display},
    rc::{Rc, Weak},
};

use mir::ir::DataType as BasicType;

use super::{
    error::{CompileError, ErrorCollector, ErrorContent},
    tokens::*,
};

#[derive(Clone, Copy)]
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

#[derive(Debug, Clone)]
#[allow(non_camel_case_types, dead_code)]
pub enum TypeExpr {
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f64,
    f32,
    usize,
    isize,
    none,

    Ptr(Box<Self>),
    Slice(Box<Self>),

    Block(Vec<(Rc<String>, TypeExpr)>, Box<Self>), // args, return type
}
impl TypeExpr {
    #[allow(unused)]
    pub fn is_equivalent(&self, rhs: &Self) -> bool {
        todo!()
    }

    pub fn as_block(&self) -> Option<(&Vec<(Rc<String>, TypeExpr)>, &Box<TypeExpr>)> {
        if let Self::Block(args, ret_type) = self {
            Some((args, ret_type))
        } else {
            None
        }
    }

    /// Returns a BasicType (aka mir::ir::DataType) if `self` is a basic type (u64, f32, i16, ...)
    /// and not a compound type (slice, struct, block, ...). If `self` is a pointer of any type
    /// returns BasicType::Unsigned64, as currently Madeline doesn't have pointer types
    pub fn into_basic_type(&self) -> Option<BasicType> {
        match self {
            Self::u64 => Some(BasicType::Unsigned64),
            Self::u32 => Some(BasicType::Unsigned32),
            Self::u16 => Some(BasicType::Unsigned16),
            Self::u8 => Some(BasicType::Unsigned8),
            Self::i64 => Some(BasicType::Signed64),
            Self::i32 => Some(BasicType::Signed32),
            Self::i16 => Some(BasicType::Signed16),
            Self::i8 => Some(BasicType::Signed8),
            Self::f64 => Some(BasicType::Float64),
            Self::f32 => Some(BasicType::Float32),
            Self::usize => Some(BasicType::UnsignedSize),
            Self::isize => Some(BasicType::SignedSize),
            Self::Ptr(..) => Some(BasicType::Pointer),
            Self::none => Some(BasicType::Irrelavent),
            _ => None,
        }
    }

    pub fn as_ptr(&self) -> Option<&Box<Self>> {
        if let Self::Ptr(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::u8 => write!(f, "u8")?,
            Self::u16 => write!(f, "u16")?,
            Self::u32 => write!(f, "u32")?,
            Self::u64 => write!(f, "u64")?,
            Self::i8 => write!(f, "i8")?,
            Self::i16 => write!(f, "i16")?,
            Self::i32 => write!(f, "i32")?,
            Self::i64 => write!(f, "i64")?,
            Self::f64 => write!(f, "f64")?,
            Self::f32 => write!(f, "f32")?,
            Self::usize => write!(f, "usize")?,
            Self::isize => write!(f, "isize")?,
            Self::none => write!(f, "none")?,
            Self::Ptr(t) => write!(f, "*{}", t)?,
            Self::Slice(t) => write!(f, "[{}]", t)?,
            Self::Block(args, ret_t) => {
                write!(f, "(")?;
                let count = args.len();
                for (i, (_, t)) in args.iter().enumerate() {
                    if i == count - 1 {
                        // is last one
                        write!(f, "{}", t)?;
                    } else {
                        write!(f, "{}, ", t)?;
                    }
                }
                write!(f, ") -> {}", ret_t)?;
            }
        }
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

    Block(Vec<Weak<ASTNode>>),
    Loop(Weak<ASTNode>),

    Return(Option<Weak<ASTNode>>),
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
    )> {
        if let Self::Def { name, dtype, rhs } = self {
            if let Some((args, ret_type)) = dtype.as_ref()?.as_block() {
                return Some((
                    name,
                    args,
                    ret_type,
                    unsafe { rhs.as_ref()?.as_ptr().as_ref()? }
                        .expr
                        .as_block()?,
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
            Self::Break => formatter.debug_tuple("Break").finish(),
            Self::Continue => formatter.debug_tuple("Continue").finish(),
            Self::Extern(arg0, arg1) => formatter
                .debug_tuple("Extern")
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
            let dtype = parse_type_expr(token_stream)?;
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
            if token_stream.peek(1).indicates_end_of_expr() {
                node = ASTNode {
                    pos,
                    expr: Expression::Return(None),
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
        _ => return Err(CompileError::unexpected_token0(token_stream.current())),
    }

    let node = if token_stream.peek(1).content == TokenContent::Squiggle {
        let pos = token_stream.next().position;
        let n = ast.add_to_node_pool(node);
        token_stream.next();
        ASTNode {
            pos,
            expr: Expression::TypeCast(n, parse_type_expr(token_stream)?),
        }
    } else {
        node
    };

    if expects_semicolon {
        if token_stream.next().content != TokenContent::Semicolon {
            return Err(CompileError::unexpected_token(
                TokenContent::Semicolon,
                token_stream.current(),
            ));
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
                type_expr = Some(parse_type_expr(token_stream)?);
                // TODO: report error when invalid type expression
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
                    token_stream.next();
                    if token_stream.current().content == TokenContent::RoundParenClose {
                        break;
                    } else if token_stream.current().content == TokenContent::Comma {
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

fn parse_type_expr(token_stream: &mut TokenStream) -> Result<TypeExpr, CompileError> {
    match &token_stream.current().content {
        TokenContent::Star => {
            token_stream.next();
            let t = parse_type_expr(token_stream)?;
            Ok(TypeExpr::Ptr(Box::new(t)))
        }
        TokenContent::RectParenOpen => {
            token_stream.next();
            let t = parse_type_expr(token_stream)?;
            if token_stream.next().content != TokenContent::RectParenClose {
                return Err(CompileError::unexpected_token(
                    TokenContent::RectParenClose,
                    token_stream.current(),
                ));
            }
            Ok(TypeExpr::Slice(Box::new(t)))
        }
        TokenContent::Identifier(id) => Ok(match id.as_str() {
            "u64" => TypeExpr::u64,
            "u32" => TypeExpr::u32,
            "u16" => TypeExpr::u16,
            "u8" => TypeExpr::u8,
            "i64" => TypeExpr::i64,
            "i32" => TypeExpr::i32,
            "i16" => TypeExpr::i16,
            "i8" => TypeExpr::i8,
            "f64" => TypeExpr::f64,
            "f32" => TypeExpr::f32,
            "usize" => TypeExpr::usize,
            "isize" => TypeExpr::isize,
            "none" => TypeExpr::none,
            _ => {
                return Err(CompileError {
                    content: ErrorContent::TypeNameNotExist(Rc::clone(id)),
                    position: token_stream.current().position,
                    length: token_stream.current().len,
                });
            }
        }),
        TokenContent::RoundParenOpen => {
            let mut args = Vec::<(Rc<String>, TypeExpr)>::new();
            let ret_type: Box<TypeExpr>;
            // parse arguments
            if token_stream.peek(1).content != TokenContent::RoundParenClose {
                loop {
                    let arg_name = if let Some(s) = token_stream.next().content.as_identifier() {
                        Rc::clone(s)
                    } else {
                        return Err(CompileError::unexpected_token(
                            TokenContent::Identifier(Rc::new(String::new())),
                            token_stream.current(),
                        ));
                    };
                    if token_stream.next().content != TokenContent::Colon {
                        return Err(CompileError::unexpected_token(
                            TokenContent::Colon,
                            token_stream.current(),
                        ));
                    }
                    token_stream.next();
                    let type_expr = parse_type_expr(token_stream)?;
                    args.push((arg_name.clone(), type_expr));
                    let t = token_stream.next();
                    if t.content == TokenContent::Comma {
                        continue;
                    } else if t.content == TokenContent::RoundParenClose {
                        break;
                    } else {
                        return Err(CompileError::unexpected_token_multiple(
                            vec![TokenContent::Comma, TokenContent::RoundParenClose],
                            token_stream.current(),
                        ));
                    }
                }
            } else {
                token_stream.next();
            }
            // parse return type
            let peek = token_stream.peek(1);
            if peek.content == TokenContent::ReturnArrow {
                token_stream.next();
                token_stream.next();
                ret_type = Box::new(parse_type_expr(token_stream)?);
            } else {
                ret_type = Box::new(TypeExpr::none);
            }
            Ok(TypeExpr::Block(args, ret_type))
        }
        _ => Err(CompileError::unexpected_token0(token_stream.current())),
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
