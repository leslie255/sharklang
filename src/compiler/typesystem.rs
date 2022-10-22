use std::{fmt::Display, rc::Rc};

use super::{ast::Expression, ast::NumValue, shir::SymbolTable};

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
