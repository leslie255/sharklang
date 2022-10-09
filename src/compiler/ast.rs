use std::{
    cell::RefCell,
    fmt::Debug,
    io::Cursor,
    num::NonZeroU16,
    rc::{Rc, Weak},
};

use mir::ir::DataType as BasicType;

use super::tokens::*;

#[derive(Clone, Copy)]
pub enum NumValue {
    U(u64),
    I(i64),
    F(f64),
}
impl std::fmt::Debug for NumValue {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U(num) => write!(formatter, "{}u", num),
            Self::I(num) => write!(formatter, "{}i", num),
            Self::F(num) => write!(formatter, "{}f", num),
        };
        return Ok(());
    }
}

impl std::fmt::Display for NumValue {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U(num) => write!(formatter, "{}u", num),
            Self::I(num) => write!(formatter, "{}i", num),
            Self::F(num) => write!(formatter, "{}f", num),
        };
        return Ok(());
    }
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
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
    none,

    Ptr(Box<Self>),
    Slice(Box<Self>),

    Block(Vec<(Rc<String>, TypeExpr)>, Box<Self>), // args, return type
}
impl TypeExpr {
    fn is_equivalent(&self, rhs: &Self) -> bool {
        todo!()
    }
    fn as_block(&self) -> Option<(&Vec<(Rc<String>, TypeExpr)>, &Box<TypeExpr>)> {
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
            Self::Ptr(..) => Some(BasicType::Unsigned64),
            _ => None,
        }
    }

    /// Returns `true` if the type expr is [`Block`].
    ///
    /// [`Block`]: TypeExpr::Block
    #[must_use]
    pub fn is_block(&self) -> bool {
        matches!(self, Self::Block(..))
    }
}

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
                let body: &Vec<Weak<ASTNode>> =
                    unsafe { rhs.as_ref()?.as_ptr().as_ref()?.expr.as_block()? };
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

pub fn parse_tokens_into_ast(token_stream: &mut TokenStream) -> AST {
    let mut ast = AST::empty();
    loop {
        if token_stream.next().content.is_eof() {
            break;
        }
        if let Some(n) = parse_expressions(&mut ast, token_stream) {
            let node = ast.add_to_node_pool(n);
            ast.root_nodes.push(node);
        }
    }
    ast
}

fn parse_expressions(ast: &mut AST, token_stream: &mut TokenStream) -> Option<ASTNode> {
    let current_token = token_stream.current();
    let mut node: Option<ASTNode>;
    match &current_token.content {
        TokenContent::Number(num_str) => {
            node = Some(ASTNode {
                pos: current_token.position,
                expr: Expression::NumLiteral(parse_numval(&num_str)?),
            });
        }
        TokenContent::String(str) => {
            node = Some(ASTNode {
                pos: current_token.position,
                expr: Expression::StrLiteral({
                    ast.strliteral_pool.push(parse_str_content(&str));
                    ast.strliteral_pool.len() - 1
                }),
            });
        }
        TokenContent::Char(val) => {
            node = Some(ASTNode {
                pos: current_token.position,
                expr: Expression::CharLiteral(*val),
            });
        }
        TokenContent::Identifier(id_name) => {
            node = parse_identifier(ast, token_stream);
        }
        TokenContent::Star => {
            let pos = current_token.position;
            token_stream.next();
            if let Some(n) = parse_expressions(ast, token_stream) {
                node = Some(ASTNode {
                    pos,
                    expr: Expression::Deref(ast.add_to_node_pool(n)),
                })
            } else {
                node = None
            }
        }
        TokenContent::And => {
            let pos = current_token.position;
            token_stream.next();
            if let Some(n) = parse_expressions(ast, token_stream) {
                node = Some(ASTNode {
                    pos,
                    expr: Expression::TakeAddr(ast.add_to_node_pool(n)),
                })
            } else {
                node = None
            }
        }
        TokenContent::BigParenOpen => {
            let pos = current_token.position;
            let mut body = Vec::<Weak<ASTNode>>::new();
            while token_stream.next().content != TokenContent::BigParenClose {
                if let Some(n) = parse_expressions(ast, token_stream) {
                    body.push(ast.add_to_node_pool(n));
                }
            }
            node = Some(ASTNode {
                pos,
                expr: Expression::Block(body),
            })
        }
        TokenContent::Return => {
            let pos = current_token.position;
            token_stream.next();
            let n = parse_expressions(ast, token_stream);
            node = Some(ASTNode {
                pos,
                expr: Expression::Return(if let Some(n) = n {
                    Some(ast.add_to_node_pool(n))
                } else {
                    None
                }),
            });
        }
        TokenContent::Continue => {
            node = Some(ASTNode {
                pos: current_token.position,
                expr: Expression::Continue,
            })
        }
        TokenContent::Break => {
            node = Some(ASTNode {
                pos: current_token.position,
                expr: Expression::Break,
            })
        }
        TokenContent::Loop => {
            let pos = current_token.position;
            token_stream.next();
            let n = parse_expressions(ast, token_stream);
            node = Some(ASTNode {
                pos,
                expr: Expression::Loop(if let Some(n) = n {
                    ast.add_to_node_pool(n)
                } else {
                    todo!();
                }),
            });
        }
        TokenContent::If => todo!(),
        TokenContent::Func => todo!(),
        TokenContent::Squiggle => node = None,
        TokenContent::SingleLineCommentStart => node = None,
        TokenContent::NewLine => node = None,
        TokenContent::RawASM(asm_code) => {
            node = Some(ASTNode {
                pos: current_token.position,
                expr: Expression::RawASM(Rc::clone(asm_code)),
            });
        }
        _ => node = None,
    }

    // parse type cast
    if let Some(node) = node {
        if token_stream.peek(1).content == TokenContent::Squiggle {
            let pos = token_stream.next().position;
            let n = ast.add_to_node_pool(node);
            token_stream.next();
            Some(ASTNode {
                pos,
                expr: Expression::TypeCast(
                    n,
                    parse_type_expr(token_stream).unwrap_or_else(|| todo!()),
                ),
            })
        } else {
            Some(node)
        }
    } else {
        None
    }
}

fn parse_identifier(ast: &mut AST, token_stream: &mut TokenStream) -> Option<ASTNode> {
    let current_token = token_stream.current();
    match token_stream.peek(1).content {
        TokenContent::Colon => {
            let pos = current_token.position;
            let name = Rc::clone(current_token.content.as_identifier()?);
            token_stream.next();
            let mut type_expr = Option::<TypeExpr>::None;
            let mut rhs = Option::<Weak<ASTNode>>::None;
            if token_stream.peek(1).content != TokenContent::Equal {
                token_stream.next();
                // the token after colon
                // if it's not equal sign means it has an explicit type expression
                type_expr = Some(parse_type_expr(token_stream).unwrap_or_else(|| todo!()));
                // TODO: report error when invalid type expression
            }
            let peek = token_stream.peek(1);
            if peek.content == TokenContent::Equal {
                token_stream.next();
                token_stream.next();
                if let Some(n) = parse_expressions(ast, token_stream) {
                    rhs = Some(ast.add_to_node_pool(n));
                }
            } else if peek.indicates_end_of_expr() {
            } else {
                todo!()
            }
            if type_expr.is_none() && rhs.is_none() {
                return None;
            }
            Some(ASTNode {
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
                expr: Expression::Identifier(Rc::clone(current_token.content.as_identifier()?)),
            };
            token_stream.next(); // equal sign
            let rhs_pos = token_stream.next().position; // token after equal sign
            let rhs_node = parse_expressions(ast, token_stream).unwrap(); // TODO: error prompt if
            let assign_expr = Expression::Assign {
                lhs: ast.add_to_node_pool(lhs_node),
                rhs: ast.add_to_node_pool(rhs_node),
            };
            Some(ASTNode {
                pos,
                expr: assign_expr,
            })
        }
        TokenContent::RoundParenOpen => {
            let pos: usize = current_token.position;
            let fn_name = Rc::clone(current_token.content.as_identifier()?);
            let mut fn_args = Vec::<Weak<ASTNode>>::new();
            token_stream.next();
            if token_stream.next().content != TokenContent::RoundParenClose {
                loop {
                    if let Some(n) = parse_expressions(ast, token_stream) {
                        fn_args.push(ast.add_to_node_pool(n));
                    }
                    token_stream.next();
                    if token_stream.current().content == TokenContent::RoundParenClose {
                        break;
                    } else if token_stream.current().content == TokenContent::Comma {
                        token_stream.next();
                    } else {
                        todo!();
                    }
                }
            }
            Some(ASTNode {
                pos,
                expr: Expression::FnCall {
                    name: fn_name,
                    args: fn_args,
                },
            })
        }
        _ => Some(ASTNode {
            pos: current_token.position,
            expr: Expression::Identifier(Rc::clone(current_token.content.as_identifier()?)),
        }),
    }
}

fn parse_type_expr(token_stream: &mut TokenStream) -> Option<TypeExpr> {
    match &token_stream.current().content {
        TokenContent::Star => {
            token_stream.next();
            let t = parse_type_expr(token_stream)?;
            Some(TypeExpr::Ptr(Box::new(t)))
        }
        TokenContent::RectParenOpen => {
            token_stream.next();
            let t = parse_type_expr(token_stream)?;
            if token_stream.next().content != TokenContent::RectParenClose {
                return None;
            }
            Some(TypeExpr::Slice(Box::new(t)))
        }
        TokenContent::Identifier(id) => Some(match id.as_str() {
            "u64" => TypeExpr::u64,
            "u32" => TypeExpr::u32,
            "u16" => TypeExpr::u16,
            "u8" => TypeExpr::u8,
            "i64" => TypeExpr::i64,
            "i32" => TypeExpr::i32,
            "i16" => TypeExpr::i16,
            "i8" => TypeExpr::i8,
            "none" => TypeExpr::none,
            _ => return None,
        }),
        TokenContent::RoundParenOpen => {
            let pos = token_stream.current().position;
            let mut args = Vec::<(Rc<String>, TypeExpr)>::new();
            let mut ret_type: Box<TypeExpr>;
            // parse arguments
            if token_stream.peek(1).content != TokenContent::RoundParenClose {
                loop {
                    let arg_name = if let Some(s) = token_stream.next().content.as_identifier() {
                        Rc::clone(s)
                    } else {
                        todo!();
                    };
                    if token_stream.next().content != TokenContent::Colon {
                        todo!();
                    }
                    token_stream.next();
                    let type_expr = if let Some(t) = parse_type_expr(token_stream) {
                        t
                    } else {
                        todo!()
                    };
                    args.push((arg_name, type_expr));
                    let t = token_stream.next();
                    if t.content == TokenContent::Comma {
                        continue;
                    } else if t.content == TokenContent::RoundParenClose {
                        break;
                    } else {
                        todo!();
                    }
                }
            }else {
                token_stream.next();
            }
            // parse return type
            let peek = token_stream.peek(1);
            if peek.content == TokenContent::ReturnArrow {
                token_stream.next();
                token_stream.next();
                ret_type = if let Some(t) = parse_type_expr(token_stream) {
                    Box::new(t)
                } else {
                    todo!();
                };
            } else {
                ret_type = Box::new(TypeExpr::none);
            }
            Some(TypeExpr::Block(args, ret_type))
        }
        _ => None,
    }
}

fn parse_str_content<'a>(src: &String) -> String {
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
                _ => new_str.push(ch),
            }
        } else {
            new_str.push(ch);
        }
    }
    new_str
}

fn parse_numval<'a>(src: &String) -> Option<NumValue> {
    if src.contains('.') {
        if let Ok(f) = src.parse::<f64>() {
            Some(NumValue::F(f))
        } else {
            None
        }
    } else if src.chars().next() == Some('-') {
        if let Ok(i) = src.parse::<i64>() {
            Some(NumValue::I(i))
        } else {
            None
        }
    } else {
        if let Ok(u) = src.parse::<u64>() {
            Some(NumValue::U(u))
        } else {
            None
        }
    }
}
