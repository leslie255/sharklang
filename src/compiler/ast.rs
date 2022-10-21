use std::{
    fmt::{Debug, Display},
    rc::{Rc, Weak},
};

use mir::ir::DataType as BasicType;

use super::{
    error::{CompileError, ErrorCollector, ErrorContent},
    shir::SymbolTable,
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

#[allow(non_camel_case_types, dead_code)]
#[derive(Debug, Clone, PartialEq)]
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

    Fn {
        args: Vec<(Rc<String>, TypeExpr)>,
        ret_type: Box<Self>,
        is_variadic: bool,
    }, // args, return type, is variadic?

    Struct(Vec<(Rc<String>, TypeExpr)>),

    TypeName(Rc<String>),
}
impl TypeExpr {
    #[must_use]
    pub fn is_float(&self, symbols: &SymbolTable) -> bool {
        match self {
            Self::u8 => false,
            Self::u16 => false,
            Self::u32 => false,
            Self::u64 => false,
            Self::i8 => false,
            Self::i16 => false,
            Self::i32 => false,
            Self::i64 => false,
            Self::f64 => true,
            Self::f32 => true,
            Self::usize => false,
            Self::isize => true,
            Self::none => false,
            Self::Ptr(_) => false,
            Self::Slice(_) => false,
            Self::Fn { .. } => false,
            Self::Struct(_) => false,
            Self::TypeName(name) => {
                if let Some((_, symbol)) = symbols.lookup(name) {
                    if let Some(t) = symbol.as_type_name() {
                        return t.is_float(symbols);
                    }
                }
                false
            }
        }
    }

    #[must_use]
    pub fn is_int(&self, symbols: &SymbolTable) -> bool {
        match self {
            Self::u8 => true,
            Self::u16 => true,
            Self::u32 => true,
            Self::u64 => true,
            Self::i8 => true,
            Self::i16 => true,
            Self::i32 => true,
            Self::i64 => true,
            Self::f64 => false,
            Self::f32 => false,
            Self::usize => true,
            Self::isize => true,
            Self::none => false,
            Self::Ptr(_) => false,
            Self::Slice(_) => false,
            Self::Fn { .. } => false,
            Self::Struct(_) => false,
            Self::TypeName(name) => {
                if let Some((_, symbol)) = symbols.lookup(name) {
                    if let Some(t) = symbol.as_type_name() {
                        return t.is_int(symbols);
                    }
                }
                false
            }
        }
    }

    #[must_use]
    pub fn matches_expr(&self, expr: &Expression, symbols: &SymbolTable) -> bool {
        macro_rules! if_none_return_false {
            ($opt: expr) => {
                if let Some(o) = $opt {
                    o
                } else {
                    return false;
                }
            };
        }
        match expr {
            Expression::Identifier(id) => {
                if_none_return_false!(if_none_return_false!(symbols.lookup(id)).1.as_variable())
                    .0
                    .is_equvalent(self, symbols)
            }
            Expression::NumLiteral(numval) => match numval {
                NumValue::U(_) | NumValue::I(_) => self.is_int(symbols),
                NumValue::F(_) => self.is_float(symbols),
            },
            Expression::StrLiteral(_) => {
                self.is_equvalent(&Self::Ptr(Box::new(TypeExpr::u8)), symbols)
            }
            Expression::CharLiteral(_) => self.is_equvalent(&Self::u8, symbols),
            Expression::Deref(child) => Self::Ptr(Box::new(self.clone()))
                .matches_expr(&if_none_return_false!(child.upgrade()).expr, symbols),
            Expression::TakeAddr(child) => {
                if let Self::Ptr(t) = self {
                    t.matches_expr(&if_none_return_false!(child.upgrade()).expr, symbols)
                } else {
                    false
                }
            }
            Expression::FnCall { name, args: _ } => {
                if_none_return_false!(if_none_return_false!(symbols.lookup(name)).1.as_function())
                    .1
                    .is_equvalent(self, symbols)
            }
            Expression::TypeCast(_, casted_type) => casted_type.is_equvalent(self, symbols),

            Expression::TypeDef(_, _) => false,
            Expression::Def { .. } => false,
            Expression::Assign { .. } => false,
            Expression::DataType(_) => false,
            Expression::Block(_) => false,
            Expression::Loop(_) => false,
            Expression::Return(_) => false,
            Expression::UnsafeReturn => false,
            Expression::Break => false,
            Expression::Continue => false,
            Expression::Extern(_, _) => false,
            Expression::RawASM(_) => false,
        }
    }

    #[must_use]
    pub fn is_equvalent(&self, rhs: &Self, symbols: &SymbolTable) -> bool {
        macro_rules! ret_false_if_none {
            ($opt: expr) => {
                if let Some(x) = $opt {
                    x
                } else {
                    return false;
                }
            };
        }
        let lhs = if let Self::TypeName(name) = self {
            ret_false_if_none!(ret_false_if_none!(symbols.lookup(name)).1.as_type_name())
        } else {
            self
        };
        let rhs = if let Self::TypeName(name) = rhs {
            ret_false_if_none!(ret_false_if_none!(symbols.lookup(name)).1.as_type_name())
        } else {
            rhs
        };
        match (lhs, rhs) {
            (Self::u8, Self::u8) => true,
            (Self::u16, Self::u16) => true,
            (Self::u32, Self::u32) => true,
            (Self::u64, Self::u64) => true,
            (Self::i8, Self::i8) => true,
            (Self::i16, Self::i16) => true,
            (Self::i32, Self::i32) => true,
            (Self::i64, Self::i64) => true,
            (Self::f64, Self::f64) => true,
            (Self::f32, Self::f32) => true,
            (Self::usize, Self::usize) => true,
            (Self::isize, Self::isize) => true,
            (Self::none, Self::none) => true,

            (Self::Ptr(lhs), Self::Ptr(rhs)) => lhs.is_equvalent(rhs, symbols),
            (Self::Slice(lhs), Self::Slice(rhs)) => lhs.is_equvalent(rhs, symbols),

            (
                Self::Fn {
                    args: lhs_args,
                    ret_type: lhs_ret_type,
                    is_variadic: lhs_is_variadic,
                },
                Self::Fn {
                    args: rhs_args,
                    ret_type: rhs_ret_type,
                    is_variadic: rhs_is_variadic,
                },
            ) => {
                if lhs_is_variadic != rhs_is_variadic {
                    return false;
                }
                if !lhs_ret_type.is_equvalent(rhs_ret_type, symbols) {
                    return false;
                }
                for (i, lhs_arg) in lhs_args.iter().enumerate() {
                    let rhs_arg = if let Some(a) = rhs_args.get(i) {
                        a
                    } else {
                        return false;
                    };
                    if !lhs_arg.1.is_equvalent(&rhs_arg.1, symbols) {
                        return false;
                    }
                }
                true
            }

            _ => false,
        }
    }

    pub fn as_block(&self) -> Option<(&Vec<(Rc<String>, TypeExpr)>, &Box<TypeExpr>, bool)> {
        if let Self::Fn {
            args,
            ret_type,
            is_variadic,
        } = self
        {
            Some((args, ret_type, *is_variadic))
        } else {
            None
        }
    }

    /// Returns a BasicType (aka mir::ir::DataType) if `self` is a basic type (u64, f32, i16, ...)
    /// and not a compound type (slice, struct, block, ...). If `self` is a pointer of any type
    /// returns BasicType::Unsigned64, as currently Madeline doesn't have pointer types
    pub fn into_basic_type(&self, symbols: &SymbolTable) -> Option<BasicType> {
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
            Self::Ptr(_) => Some(BasicType::Pointer),
            Self::none => Some(BasicType::Irrelavent),
            Self::TypeName(name) => symbols
                .lookup(name)?
                .1
                .as_type_name()?
                .into_basic_type(symbols),
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
            Self::Fn {
                args,
                ret_type,
                is_variadic,
            } => {
                write!(f, "(")?;
                let count = args.len();
                for (i, (_, t)) in args.iter().enumerate() {
                    if i == count - 1 {
                        // is last one
                        write!(f, "{t}")?;
                    } else {
                        write!(f, "{t}, ")?;
                    }
                }
                if *is_variadic {
                    write!(f, "..) -> {ret_type}")?;
                } else {
                    write!(f, ") -> {ret_type}")?;
                }
            }
            Self::Struct(fields) => {
                write!(f, "struct {{ ")?;
                let count = fields.len();
                for (i, (id, t)) in fields.iter().enumerate() {
                    if i == count - 1 {
                        // is last one
                        write!(f, "{id}: {t}")?;
                    } else {
                        write!(f, "{id}: {t}, ")?;
                    }
                }
                write!(f, " }}")?;
            }
            Self::TypeName(name) => write!(f, "`{name}`")?,
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
            let rhs_type = parse_type_expr(token_stream)?;
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
            expr: Expression::TypeCast(n, parse_type_expr(token_stream)?),
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
            _ => TypeExpr::TypeName(Rc::clone(id)),
        }),
        TokenContent::RoundParenOpen => {
            let mut is_variadic = false;
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
                    } else if t.content == TokenContent::DotDot {
                        is_variadic = true;
                        if token_stream.next().content != TokenContent::RoundParenClose {
                            return Err(CompileError::unexpected_token(
                                TokenContent::DotDot,
                                token_stream.current(),
                            ));
                        }
                        break;
                    } else {
                        return Err(CompileError::unexpected_token_multiple(
                            vec![
                                TokenContent::Comma,
                                TokenContent::RoundParenClose,
                                TokenContent::DotDot,
                            ],
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
            Ok(TypeExpr::Fn {
                args,
                ret_type,
                is_variadic,
            })
        }
        TokenContent::BigParenOpen => {
            if token_stream.peek(1).content == TokenContent::BigParenClose {
                token_stream.next();
                return Ok(TypeExpr::Struct(Vec::new()));
            }
            let mut fields = Vec::<(Rc<String>, TypeExpr)>::new();
            loop {
                let field_name = token_stream.next().expects_identifier()?;
                let field_name = Rc::clone(field_name);
                if token_stream.next().content != TokenContent::Colon {
                    return Err(CompileError::unexpected_token(
                        TokenContent::Colon,
                        token_stream.current(),
                    ));
                }
                token_stream.next();
                let field_type = parse_type_expr(token_stream)?;
                fields.push((field_name, field_type));
                let pclose_or_comma = token_stream.next().content.clone();
                if pclose_or_comma == TokenContent::BigParenClose {
                    return Ok(TypeExpr::Struct(fields));
                } else if pclose_or_comma == TokenContent::Comma {
                    continue;
                } else {
                    return Err(CompileError::unexpected_token_multiple(
                        vec![TokenContent::Comma, TokenContent::BigParenClose],
                        token_stream.current(),
                    ));
                }
            }
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
