use std::{fmt::Display, rc::Rc};

use crate::compiler::ast::DerefToASTNode;

use super::{
    ast::Expression,
    ast::{ASTNode, NumValue},
    error::{CompileError, ErrorCollector, ErrorContent},
    shir::SymbolTable,
    tokens::{TokenContent, TokenStream},
};

use mir::ir::DataType as BasicType;

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
        match expr {
            Expression::Identifier(id) => || -> Option<bool> {
                Some(
                    symbols
                        .lookup(id)?
                        .1
                        .as_variable()?
                        .0
                        .is_equvalent(self, symbols),
                )
            }()
            .unwrap_or(false),
            Expression::NumLiteral(numval) => match numval {
                NumValue::U(_) | NumValue::I(_) => self.is_int(symbols),
                NumValue::F(_) => self.is_float(symbols),
            },
            Expression::StrLiteral(_) => {
                self.is_equvalent(&Self::Ptr(Box::new(TypeExpr::u8)), symbols)
            }
            Expression::CharLiteral(_) => self.is_equvalent(&Self::u8, symbols),
            Expression::BoolLiteral(_) => self.is_equvalent(&Self::u8, symbols),
            Expression::Deref(child) => {
                Self::Ptr(Box::new(self.clone())).matches_expr(&child.deref().expr, symbols)
            }
            Expression::TakeAddr(child) => {
                if let Self::Ptr(t) = self {
                    t.matches_expr(&child.deref().expr, symbols)
                } else {
                    false
                }
            }
            Expression::FnCall { name, args: _ } => || -> Option<bool> {
                Some(
                    symbols
                        .lookup(name)?
                        .1
                        .as_function()?
                        .1
                        .is_equvalent(self, symbols),
                )
            }()
            .unwrap_or(false),
            Expression::TypeCast(_, casted_type) => casted_type.is_equvalent(self, symbols),
            Expression::Cmp(_, _, _) => self.is_int(symbols),
            Expression::TypeDef(_, _) => false,
            Expression::Def { .. } => false,
            Expression::Assign { .. } => false,
            Expression::DataType(_) => false,
            Expression::Block(_) => false,
            Expression::Loop(_) => false,
            Expression::If { .. } => false,
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

            (Self::Ptr(lhs), Self::Ptr(rhs)) => lhs.is_equvalent(&rhs, symbols),
            (Self::Slice(lhs), Self::Slice(rhs)) => lhs.is_equvalent(&rhs, symbols),

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
                if !lhs_ret_type.is_equvalent(&rhs_ret_type, symbols) {
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

    /// Returns `true` if the type expr is [`none`].
    ///
    /// [`none`]: TypeExpr::none
    #[must_use]
    pub fn is_none(&self) -> bool {
        matches!(self, Self::none)
    }

    pub fn parse_from_tokens(token_stream: &mut TokenStream) -> Result<TypeExpr, CompileError> {
        match &token_stream.current().content {
            TokenContent::Star => {
                token_stream.next();
                let t = TypeExpr::parse_from_tokens(token_stream)?;
                Ok(TypeExpr::Ptr(Box::new(t)))
            }
            TokenContent::RectParenOpen => {
                token_stream.next();
                let t = TypeExpr::parse_from_tokens(token_stream)?;
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
            TokenContent::ReturnArrow => {
                token_stream.next();
                let ret_type = TypeExpr::parse_from_tokens(token_stream)?;
                Ok(TypeExpr::Fn {
                    args: Vec::new(),
                    ret_type: Box::new(ret_type),
                    is_variadic: false,
                })
            }
            TokenContent::RoundParenOpen => {
                let mut is_variadic = false;
                let mut args = Vec::<(Rc<String>, TypeExpr)>::new();
                let ret_type: Box<TypeExpr>;
                // parse arguments
                if token_stream.peek(1).content != TokenContent::RoundParenClose {
                    loop {
                        let arg_name = if let Some(s) = token_stream.next().content.as_identifier()
                        {
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
                        let type_expr = TypeExpr::parse_from_tokens(token_stream)?;
                        args.push((arg_name.clone(), type_expr));
                        let t = token_stream.next();
                        if t.content == TokenContent::Comma {
                            if token_stream.peek(1).content == TokenContent::RoundParenClose {
                                token_stream.next();
                                break;
                            }
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
                    ret_type = Box::new(TypeExpr::parse_from_tokens(token_stream)?);
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
                    let field_type = TypeExpr::parse_from_tokens(token_stream)?;
                    fields.push((field_name, field_type));
                    let token = token_stream.next().content.clone();
                    if token == TokenContent::BigParenClose {
                        return Ok(TypeExpr::Struct(fields));
                    } else if token == TokenContent::Comma {
                        if token_stream.peek(1).content == TokenContent::BigParenClose {
                            token_stream.next();
                            return Ok(TypeExpr::Struct(fields));
                        }
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

#[must_use]
pub fn suggest_typeexpr(expr: &Expression, symbols: &SymbolTable) -> Option<TypeExpr> {
    match expr {
        Expression::Identifier(id) => Some(symbols.lookup(id)?.1.as_variable()?.0.clone()),
        Expression::NumLiteral(num) => Some(match num {
            NumValue::U(_) => TypeExpr::usize,
            NumValue::I(_) => TypeExpr::isize,
            NumValue::F(_) => TypeExpr::f64,
        }),
        Expression::StrLiteral(_) => Some(TypeExpr::Ptr(Box::new(TypeExpr::u8))),
        Expression::CharLiteral(_) => Some(TypeExpr::u8),
        Expression::BoolLiteral(_) => Some(TypeExpr::u8),
        Expression::FnCall { name, args: _ } => {
            Some(symbols.lookup(name)?.1.as_function()?.1.clone())
        }
        Expression::Deref(child) => Some(
            *suggest_typeexpr(&child.deref().expr, symbols)?
                .as_ptr()?
                .clone(),
        ),
        Expression::TakeAddr(child) => Some(TypeExpr::Ptr(Box::new(suggest_typeexpr(
            &child.deref().expr,
            symbols,
        )?))),
        Expression::TypeCast(_, t) => Some(t.clone()),
        Expression::Block(_) => Some(TypeExpr::Fn {
            args: Vec::new(),
            ret_type: Box::new(TypeExpr::none),
            is_variadic: false,
        }),
        _ => None,
    }
}

pub fn check_type(
    expected_type: &TypeExpr,
    node: &ASTNode,
    err_collector: &mut ErrorCollector,
    symbols: &SymbolTable,
) {
    if !expected_type.matches_expr(&node.expr, symbols) {
        err_collector.errors.push(CompileError {
            content: ErrorContent::MismatchedType {
                expected: expected_type.clone(),
                found: suggest_typeexpr(&node.expr, symbols),
            },
            position: node.pos,
            length: 1,
        })
    }
}
