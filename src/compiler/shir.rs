use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;

use mir::ir::DataType as BasicType;

use super::ast::*;

#[derive(Debug, Clone, Default)]
pub struct SHIRProgram {
    pub body: Vec<SHIRTopLevel>,
    pub strliteral_pool: Vec<String>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum SHIRTopLevel {
    Fn {
        is_local: bool,
        name: Rc<String>,
        body: Vec<SHIR>,
        ret_type: BasicType,
    },
    StaticVar {
        name: Rc<String>,
        val: SHIRConst,
    },
    ExternFn {
        name: Rc<String>,
        ret_type: BasicType,
    },
}

#[derive(Debug, Clone)]
pub enum SHIRConst {
    Number(NumValue),
    String(usize),
    Char(u8),
}

#[derive(Debug, Clone)]
pub enum SHIR {
    Var(u64, BasicType),
    Const(SHIRConst),
    Arg(u64, BasicType),
    VarAssign {
        id: u64,
        dtype: BasicType,
        rhs: Box<Self>,
    },
    VarDef {
        id: u64,
        dtype: BasicType,
    },
    FnCall {
        name: Rc<String>,
        args: Vec<(Self, BasicType)>, // argument, expected_type
        ret_type: BasicType,
    },
    ReturnVoid,
    ReturnValue(Box<Self>),
    Deref(Box<SHIR>, BasicType),
    TakeAddr(Box<SHIR>),
}

impl SHIR {
    fn type_cast(&mut self, t: BasicType) {
        match self {
            SHIR::Var(_, dtype)
            | SHIR::VarAssign {
                id: _,
                dtype,
                rhs: _,
            }
            | SHIR::Arg(_, dtype)
            | SHIR::VarDef { id: _, dtype }
            | SHIR::FnCall {
                name: _,
                args: _,
                ret_type: dtype,
            }
            | SHIR::Deref(_, dtype) => *dtype = t,
            SHIR::Const(_) | SHIR::ReturnVoid | SHIR::ReturnValue(_) | Self::TakeAddr(_) => (),
        }
    }
    #[must_use]
    fn as_string(&self) -> Option<usize> {
        if let SHIR::Const(c) = self {
            if let SHIRConst::String(s) = c {
                Some(*s)
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    global: HashMap<Rc<String>, Symbol>,
    local: HashMap<Rc<String>, Symbol>,
}

impl SymbolTable {
    fn lookup(&self, id: &Rc<String>) -> Option<(&Rc<String>, &Symbol)> {
        if let Some(s) = self.local.get_key_value(id) {
            Some(s)
        } else if let Some(s) = self.global.get_key_value(id) {
            Some(s)
        } else {
            None
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Symbol {
    Function(Vec<TypeExpr>, TypeExpr), // name, expected type
    Variable(TypeExpr, u64),           // type, id
    TypeName(TypeExpr),
}

impl Symbol {
    pub fn as_function(&self) -> Option<(&Vec<TypeExpr>, &TypeExpr)> {
        if let Self::Function(a, b) = self {
            Some((a, b))
        } else {
            None
        }
    }
    pub fn as_variable(&self) -> Option<(&TypeExpr, u64)> {
        if let Self::Variable(a, b) = self {
            Some((a, *b))
        } else {
            None
        }
    }
    #[allow(dead_code)]
    pub fn as_type_name(&self) -> Option<&TypeExpr> {
        if let Self::TypeName(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub fn ast_into_shir(ast: AST) -> SHIRProgram {
    let mut program = SHIRProgram::default();
    let mut symbols = SymbolTable::default();

    // for the implicit function call syntax
    let println_str = Rc::new("println".to_string());

    for (i, root_node) in ast
        .root_nodes
        .iter()
        .filter_map(|w| w.upgrade())
        .enumerate()
    {
        if let Some((name, args, ret_type, body)) = root_node.expr.as_fn_def() {
            // --- if it is a function definition
            symbols.global.insert(
                Rc::clone(name),
                Symbol::Function(
                    args.iter().map(|(_, t)| t.clone()).collect(),
                    ret_type.clone(),
                ),
            );

            let mut fn_body = Vec::<SHIR>::new();
            // load arguments into variables
            for (i, (arg_name, arg_t)) in args.iter().enumerate() {
                let arg_id = {
                    let mut hasher = DefaultHasher::new();
                    arg_name.hash(&mut hasher);
                    hasher.finish()
                };
                symbols
                    .local
                    .insert(Rc::clone(arg_name), Symbol::Variable(arg_t.clone(), arg_id));
                let arg_t_basic = arg_t.into_basic_type().unwrap();
                fn_body.push(SHIR::VarDef {
                    id: arg_id,
                    dtype: arg_t_basic,
                });
                fn_body.push(SHIR::VarAssign {
                    id: arg_id,
                    dtype: arg_t_basic,
                    rhs: Box::new(SHIR::Arg(i as u64, arg_t_basic)),
                });
            }
            // convert body
            for expr in body
                .iter()
                .filter_map(|w| unsafe { w.as_ptr().as_ref() })
                .map(|n| &n.expr)
            {
                let s = convert_body(expr, &mut fn_body, &mut symbols, i);
                if let Some(s) = s {
                    if let Some(str_id) = s.as_string() {
                        fn_body.push(SHIR::FnCall {
                            name: Rc::clone(&println_str),
                            args: vec![(
                                SHIR::Const(SHIRConst::String(str_id)),
                                BasicType::Unsigned64,
                            )],
                            ret_type: BasicType::Irrelavent,
                        })
                    } else {
                        fn_body.push(s);
                    }
                } else {
                    panic!("failed to convert to SHIR: {:?}", expr);
                }
            }
            symbols.local.clear();
            program.body.push(SHIRTopLevel::Fn {
                is_local: false,
                name: Rc::clone(name),
                body: fn_body,
                ret_type: ret_type.into_basic_type().unwrap(), // TODO: return structures
            });
        } else if let Some((name, dtype)) = root_node.expr.as_extern() {
            let (args, ret_type) = dtype.as_block().unwrap();
            symbols.global.insert(
                Rc::clone(name),
                Symbol::Function(
                    args.iter().map(|(_, t)| t.clone()).collect(),
                    *ret_type.clone(),
                ),
            );
            program.body.push(SHIRTopLevel::ExternFn {
                name: Rc::clone(name),
                ret_type: ret_type.into_basic_type().unwrap(),
            })
        }
    }
    program.strliteral_pool = ast.strliteral_pool;
    program
}

fn convert_body(
    expr: &Expression,
    parent: &mut Vec<SHIR>,
    symbols: &mut SymbolTable,
    i: usize,
) -> Option<SHIR> {
    match expr {
        Expression::Identifier(id) => {
            let (dtype, num_id) = symbols.lookup(id)?.1.as_variable()?;
            Some(SHIR::Var(num_id, dtype.into_basic_type()?))
        }
        Expression::NumLiteral(val) => Some(SHIR::Const(SHIRConst::Number(*val))),
        Expression::StrLiteral(str) => Some(SHIR::Const(SHIRConst::String(*str))),
        Expression::CharLiteral(ch) => Some(SHIR::Const(SHIRConst::Char(*ch))),
        Expression::Def { name, dtype, rhs } => {
            let dtype = if let Some(t) = dtype {
                t.clone()
            } else {
                suggest_typeexpr(&rhs.as_ref()?.upgrade()?.expr, symbols)?
            };
            let var_id = {
                let mut hasher = DefaultHasher::new();
                name.hash(&mut hasher);
                hasher.finish()
            };
            symbols
                .local
                .insert(Rc::clone(name), Symbol::Variable(dtype.clone(), var_id));
            let var_def = SHIR::VarDef {
                id: var_id,
                dtype: dtype.into_basic_type().unwrap_or_else(|| todo!()),
            };
            if let Some(rhs) = rhs {
                parent.push(var_def);
                let rhs_shir = convert_body(&rhs.upgrade()?.expr, parent, symbols, i)?;
                Some(SHIR::VarAssign {
                    id: var_id,
                    dtype: dtype.into_basic_type()?,
                    rhs: Box::new(rhs_shir),
                })
            } else {
                Some(var_def)
            }
        }
        Expression::Assign { lhs, rhs } => {
            let lhs_expr = &lhs.upgrade()?.expr;
            if let Some((_, symbol)) = symbols.lookup(lhs_expr.as_identifier()?) {
                let (var_type, var_id) = symbol.as_variable()?;
                Some(SHIR::VarAssign {
                    id: var_id,
                    dtype: var_type.into_basic_type()?,
                    rhs: Box::new(convert_body(&rhs.upgrade()?.expr, parent, symbols, i)?),
                })
            } else if let Some(_) = lhs_expr.as_deref() {
                todo!()
            } else {
                None
            }
        }
        Expression::FnCall { name, args } => {
            let mut args_shir = Vec::<(SHIR, BasicType)>::new();
            let (name, symbol) = symbols.lookup(name)?;
            let name = Rc::clone(name);
            let symbol = symbol.clone();
            let (arg_types, ret_type) = symbol.as_function()?;
            for (j, arg) in args.iter().enumerate() {
                let mut arg_shir = convert_body(&arg.upgrade()?.expr, parent, symbols, i + 1 + j)?;
                if let SHIR::FnCall {
                    name: _,
                    args: _,
                    ret_type,
                } = &arg_shir
                {
                    // recursive function call,
                    // declare another variable before function call, assign it with the function
                    // essentially expanding `f(g(x))` into...
                    // temp: = g(x); f(temp);
                    let temp_var_id = {
                        let mut hasher = DefaultHasher::new();
                        i.hash(&mut hasher);
                        j.hash(&mut hasher);
                        hasher.finish()
                    };
                    parent.push(SHIR::VarDef {
                        id: temp_var_id,
                        dtype: *ret_type,
                    });
                    parent.push(SHIR::VarAssign {
                        id: temp_var_id,
                        dtype: *ret_type,
                        rhs: Box::new(arg_shir.clone()),
                    });
                    arg_shir = SHIR::Var(temp_var_id, *ret_type);
                }
                args_shir.push((arg_shir, arg_types[j].into_basic_type()?));
            }
            Some(SHIR::FnCall {
                name,
                args: args_shir,
                ret_type: ret_type.into_basic_type()?,
            })
        }
        Expression::Deref(child) => {
            let child_expr = &child.upgrade().unwrap().expr;
            let s = convert_body(child_expr, parent, symbols, i)?;
            let dtype = if let Some(TypeExpr::Ptr(t)) = suggest_typeexpr(child_expr, symbols) {
                t
            } else {
                panic!("dereferencing expression of non-pointer type")
            };
            Some(SHIR::Deref(Box::new(s), dtype.into_basic_type()?))
        }
        Expression::TakeAddr(child) => {
            let child_expr = &child.upgrade().unwrap().expr;
            let s = convert_body(child_expr, parent, symbols, i)?;
            Some(SHIR::TakeAddr(Box::new(s)))
        }
        Expression::DataType(_) => None,
        Expression::TypeCast(n, t) => {
            let child = &unsafe { n.as_ptr().as_ref()? }.expr;
            let mut s = convert_body(child, parent, symbols, i)?;
            s.type_cast(t.into_basic_type()?);
            Some(s)
        }
        Expression::Block(_) => None,
        Expression::Loop(_) => todo!(),
        Expression::Return(node) => {
            if let Some(node) = node {
                let node = &unsafe { node.as_ptr().as_ref()? }.expr;
                let s = convert_body(node, parent, symbols, i)?;
                Some(SHIR::ReturnValue(Box::new(s)))
            } else {
                Some(SHIR::ReturnVoid)
            }
        }
        Expression::Break => todo!(),
        Expression::Continue => todo!(),
        Expression::Extern(_, _) => panic!(),
        Expression::RawASM(_) => todo!(),
    }
}

#[must_use]
fn suggest_typeexpr(expr: &Expression, symbols: &SymbolTable) -> Option<TypeExpr> {
    match expr {
        Expression::Identifier(id) => Some(symbols.lookup(id)?.1.as_variable()?.0.clone()),
        Expression::NumLiteral(num) => Some(match num {
            NumValue::U(_) => TypeExpr::usize,
            NumValue::I(_) => TypeExpr::isize,
            NumValue::F(_) => TypeExpr::f64,
        }),
        Expression::StrLiteral(_) => Some(TypeExpr::Ptr(Box::new(TypeExpr::u8))),
        Expression::CharLiteral(_) => Some(TypeExpr::u8),
        Expression::FnCall { name, args: _ } => {
            Some(symbols.lookup(name)?.1.as_function()?.1.clone())
        }
        Expression::Deref(child) => Some(
            *suggest_typeexpr(&child.upgrade()?.expr, symbols)?
                .as_ptr()?
                .clone(),
        ),
        Expression::TakeAddr(child) => Some(TypeExpr::Ptr(Box::new(suggest_typeexpr(
            &child.upgrade()?.expr,
            symbols,
        )?))),
        Expression::TypeCast(_, t) => Some(t.clone()),
        Expression::Block(_) => Some(TypeExpr::Block(Vec::new(), Box::new(TypeExpr::none))),
        _ => None,
    }
}
