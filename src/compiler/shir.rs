use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;
use std::rc::Weak;

use mir::ir::DataType as BasicType;

use super::ast::*;
use super::tokens::*;

#[derive(Debug, Clone, Default)]
pub struct SHIRProgram {
    pub body: Vec<SHIRTopLevel>,
    pub strliteral_pool: Vec<String>,
}

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
        args: Vec<Self>,
        ret_type: BasicType,
    },
    ReturnVoid,
    ReturnValue(Box<Self>),
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
            | SHIR::VarDef { id: _, dtype }
            | SHIR::FnCall {
                name: _,
                args: _,
                ret_type: dtype,
            } => *dtype = t,
            SHIR::Const(_) | SHIR::ReturnVoid | SHIR::ReturnValue(_) => (),
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

#[derive(Debug, Clone)]
pub enum Symbol {
    Function(TypeExpr),
    Variable(TypeExpr, u64),
    TypeName(TypeExpr),
}

impl Symbol {
    pub fn as_function(&self) -> Option<&TypeExpr> {
        if let Self::Function(v) = self {
            Some(v)
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
    for (i, root_node) in ast
        .root_nodes
        .iter()
        .filter_map(|w| w.upgrade())
        .enumerate()
    {
        if let Some((name, args, ret_type, body)) = root_node.expr.as_fn_def() {
            // --- if it is a function definition
            symbols
                .global
                .insert(Rc::clone(name), Symbol::Function(ret_type.clone()));

            let mut fn_body = Vec::<SHIR>::new();
            for expr in body
                .iter()
                .filter_map(|w| unsafe { w.as_ptr().as_ref() })
                .map(|n| &n.expr)
            {
                let s = convert_body(expr, &mut fn_body, &mut symbols, i);
                if let Some(s) = s {
                    fn_body.push(s);
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
                t
            } else {
                return None;
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
            if let Some((name, symbol)) = symbols.lookup(lhs_expr.as_identifier()?) {
                let (var_type, var_id) = symbol.as_variable()?;
                Some(SHIR::VarAssign {
                    id: var_id,
                    dtype: var_type.into_basic_type()?,
                    rhs: Box::new(convert_body(&rhs.upgrade()?.expr, parent, symbols, i)?),
                })
            } else if let Some(deref) = lhs_expr.as_deref() {
                todo!()
            } else {
                None
            }
        }
        Expression::FnCall { name, args } => {
            let mut args_shir = Vec::<SHIR>::new();
            for (j, arg) in args.iter().enumerate() {
                let mut arg_shir = convert_body(&arg.upgrade()?.expr, parent, symbols, i)?;
                if let SHIR::FnCall {
                    name,
                    args,
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
                args_shir.push(arg_shir);
            }
            let (name, symbol) = symbols.lookup(name)?;
            let ret_type = symbol.as_function()?.into_basic_type()?;
            Some(SHIR::FnCall {
                name: Rc::clone(name),
                args: args_shir,
                ret_type,
            })
        }
        Expression::Deref(_) => todo!(),
        Expression::TakeAddr(_) => todo!(),
        Expression::DataType(_) => None,
        Expression::TypeCast(n, t) => {
            let child = &unsafe { n.as_ptr().as_ref()? }.expr;
            let mut s = convert_body(child, parent, symbols, i)?;
            s.type_cast(t.into_basic_type()?);
            Some(s)
        }
        Expression::Block(_) => None,
        Expression::Loop(body) => todo!(),
        Expression::Return(node) => {
            if let Some(node) = node {
                let node = &unsafe { node.as_ptr().as_ref()? }.expr;
                let mut s = convert_body(node, parent, symbols, i)?;
                Some(SHIR::ReturnValue(Box::new(s)))
            } else {
                Some(SHIR::ReturnVoid)
            }
        }
        Expression::Break => todo!(),
        Expression::Continue => todo!(),
        Expression::RawASM(_) => todo!(),
    }
}
