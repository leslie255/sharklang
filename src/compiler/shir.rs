use super::{
    ast::{ASTNode, Expression, NumValue, AST},
    error::{CompileError, ErrorCollector, ErrorContent},
    typesystem::{check_type, suggest_typeexpr, TypeExpr},
};
use mir::ir::DataType as BasicType;
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    rc::{Rc, Weak},
};

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

#[derive(Debug, Clone, PartialEq)]
pub enum SHIRConst {
    Number(NumValue),
    String(usize),
    Char(u8),
}

#[derive(Debug, Clone, PartialEq)]
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
    RawASM(Rc<String>),
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
            SHIR::Const(_)
            | SHIR::ReturnVoid
            | SHIR::ReturnValue(_)
            | Self::TakeAddr(_)
            | Self::RawASM(_) => (),
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
    pub fn lookup(&self, id: &Rc<String>) -> Option<(&Rc<String>, &Symbol)> {
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
    Function(Vec<TypeExpr>, TypeExpr, bool), // args, return type, is variadic?
    Variable(TypeExpr, u64),                 // type, id
    TypeName(TypeExpr),                      // type content
}

impl Symbol {
    pub fn as_function(&self) -> Option<(&Vec<TypeExpr>, &TypeExpr, bool)> {
        if let Self::Function(a, b, c) = self {
            Some((a, b, *c))
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

#[derive(Debug, Clone)]
struct Context {
    symbols: SymbolTable,
    parent_fn_ret_type: Option<TypeExpr>,
    // parent_loop: ...
    has_ret_statement: bool,
}

pub fn ast_into_shir(ast: AST, err_collector: &mut ErrorCollector) -> SHIRProgram {
    let mut program = SHIRProgram::default();
    let mut context = Context {
        symbols: SymbolTable::default(),
        parent_fn_ret_type: None,
        has_ret_statement: false,
    };

    for (i, root_node) in ast
        .root_nodes
        .iter()
        .filter_map(|w| w.upgrade())
        .enumerate()
    {
        if let Some((name, args, ret_type, body, is_variadic)) = root_node.expr.as_fn_def() {
            // --- if it is a function definition

            context.parent_fn_ret_type = Some(ret_type.clone());
            context.symbols.global.insert(
                Rc::clone(name),
                Symbol::Function(
                    args.iter().map(|(_, t)| t.clone()).collect(),
                    ret_type.clone(),
                    is_variadic,
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
                context
                    .symbols
                    .local
                    .insert(Rc::clone(arg_name), Symbol::Variable(arg_t.clone(), arg_id));
                let arg_t_basic = arg_t.into_basic_type(&context.symbols).unwrap();
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
            convert_block(body, &mut context, i, err_collector, &mut fn_body);
            check_return(
                ret_type,
                &mut fn_body,
                &root_node,
                &mut context,
                err_collector,
            );
            context.symbols.local.clear();
            program.body.push(SHIRTopLevel::Fn {
                is_local: false,
                name: Rc::clone(name),
                body: fn_body,
                ret_type: ret_type.into_basic_type(&context.symbols).unwrap(), // TODO: return structures
            });
        } else if let Some((name, dtype)) = root_node.expr.as_extern() {
            let (args, ret_type, is_variadic) = dtype.as_block().unwrap();
            context.symbols.global.insert(
                Rc::clone(name),
                Symbol::Function(
                    args.iter().map(|(_, t)| t.clone()).collect(),
                    *ret_type.clone(),
                    is_variadic,
                ),
            );
            program.body.push(SHIRTopLevel::ExternFn {
                name: Rc::clone(name),
                ret_type: ret_type.into_basic_type(&context.symbols).unwrap(),
            })
        } else if let Expression::TypeDef(name, rhs_type) = &root_node.expr {
            context
                .symbols
                .global
                .insert(Rc::clone(name), Symbol::TypeName(rhs_type.clone()));
        }
    }
    program.strliteral_pool = ast.strliteral_pool;
    program
}

fn convert_block(
    body: &Vec<Weak<ASTNode>>,
    context: &mut Context,
    i: usize,
    err_collector: &mut ErrorCollector,
    target: &mut Vec<SHIR>,
) {
    context.has_ret_statement = false;
    for node in body.iter().filter_map(|w| unsafe { w.as_ptr().as_ref() }) {
        let s = convert_body(node, target, context, i, err_collector);
        if let Some(s) = s {
            if let SHIR::ReturnValue(_) = s {
                context.has_ret_statement = true;
            } else if SHIR::ReturnVoid == s {
                context.has_ret_statement = true;
            }
            if let Some(str_id) = s.as_string() {
                handle_implicit_printf(context, err_collector, node, target, str_id);
            } else {
                target.push(s);
            }
        }
    }
}

fn convert_body(
    node: &ASTNode,
    parent: &mut Vec<SHIR>,
    context: &mut Context,
    i: usize,
    err_collector: &mut ErrorCollector,
) -> Option<SHIR> {
    match &node.expr {
        Expression::Identifier(id) => {
            let (dtype, num_id) = context.symbols.lookup(id)?.1.as_variable()?;
            Some(SHIR::Var(num_id, dtype.into_basic_type(&context.symbols)?))
        }
        Expression::NumLiteral(val) => Some(SHIR::Const(SHIRConst::Number(*val))),
        Expression::StrLiteral(str) => Some(SHIR::Const(SHIRConst::String(*str))),
        Expression::CharLiteral(ch) => Some(SHIR::Const(SHIRConst::Char(*ch))),
        Expression::Def { name, dtype, rhs } => {
            let dtype = if let Some(t) = dtype {
                // check if rhs matches type
                if let Some(rhs) = rhs {
                    let rhs_node = &rhs.upgrade()?;
                    check_type(t, rhs_node, err_collector, &context.symbols);
                }
                t.clone()
            } else {
                suggest_typeexpr(&rhs.as_ref()?.upgrade()?.expr, &context.symbols)?
            };
            let var_id = {
                let mut hasher = DefaultHasher::new();
                name.hash(&mut hasher);
                hasher.finish()
            };
            context
                .symbols
                .local
                .insert(Rc::clone(name), Symbol::Variable(dtype.clone(), var_id));
            let var_def = SHIR::VarDef {
                id: var_id,
                dtype: dtype.into_basic_type(&context.symbols)?,
            };
            if let Some(rhs) = rhs {
                parent.push(var_def);
                let rhs_shir = convert_body(
                    unsafe { rhs.as_ptr().as_ref()? },
                    parent,
                    context,
                    i,
                    err_collector,
                )?;
                Some(SHIR::VarAssign {
                    id: var_id,
                    dtype: dtype.into_basic_type(&context.symbols)?,
                    rhs: Box::new(rhs_shir),
                })
            } else {
                Some(var_def)
            }
        }
        Expression::Assign { lhs, rhs } => {
            let lhs_expr = &lhs.upgrade()?.expr;
            if let Some((_, symbol)) = context.symbols.lookup(lhs_expr.as_identifier()?) {
                let rhs_node = &rhs.upgrade()?;
                let (var_type, var_id) = symbol.as_variable()?;
                check_type(var_type, rhs_node, err_collector, &context.symbols);
                Some(SHIR::VarAssign {
                    id: var_id,
                    dtype: var_type.into_basic_type(&context.symbols)?,
                    rhs: Box::new(convert_body(&rhs_node, parent, context, i, err_collector)?),
                })
            } else if let Some(_) = lhs_expr.as_deref() {
                todo!()
            } else {
                None
            }
        }
        Expression::FnCall { name, args } => {
            let mut args_shir = Vec::<(SHIR, BasicType)>::new();
            let (name, symbol) = context.symbols.lookup(name)?;
            let name = Rc::clone(name);
            let symbol = symbol.clone();
            let (arg_types, ret_type, is_variadic) = symbol.as_function()?;
            // check argument count
            let expected_arg_count = arg_types.len();
            let actual_arg_count = args.len();
            if !is_variadic {
                if expected_arg_count != actual_arg_count {
                    err_collector.errors.push(CompileError {
                        content: ErrorContent::IncorrectArgCount {
                            expected: expected_arg_count,
                            found: actual_arg_count,
                        },
                        position: node.pos + name.len(),
                        length: 1,
                    });
                    return None;
                }
            } else {
                let expected_arg_count = expected_arg_count - 1;
                if expected_arg_count > actual_arg_count {
                    err_collector.errors.push(CompileError {
                        content: ErrorContent::IncorrectArgCountVariadic {
                            expected: expected_arg_count,
                            found: actual_arg_count,
                        },
                        position: node.pos + name.len(),
                        length: 1,
                    });
                    return None;
                }
            }
            for (j, arg) in args.iter().enumerate() {
                let arg_type = &arg_types.get(j).unwrap_or(arg_types.last().unwrap());
                let arg_node = &arg.upgrade()?;
                check_type(arg_type, arg_node, err_collector, &context.symbols);
                let mut arg_shir =
                    convert_body(&arg_node, parent, context, i + 1 + j, err_collector)?;
                if let SHIR::FnCall {
                    name: _,
                    args: _,
                    ret_type,
                } = &arg_shir
                {
                    // using the return value of a function as the argument of this function,
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
                args_shir.push((arg_shir, arg_type.into_basic_type(&context.symbols)?));
            }
            Some(SHIR::FnCall {
                name,
                args: args_shir,
                ret_type: ret_type.into_basic_type(&context.symbols)?,
            })
        }
        Expression::Deref(child) => {
            let child_node = &child.upgrade().unwrap();
            let s = convert_body(&child_node, parent, context, i, err_collector)?;
            let dtype = if let Some(TypeExpr::Ptr(t)) =
                suggest_typeexpr(&child_node.expr, &context.symbols)
            {
                t
            } else {
                panic!("dereferencing expression of non-pointer type")
            };
            Some(SHIR::Deref(
                Box::new(s),
                dtype.into_basic_type(&context.symbols)?,
            ))
        }
        Expression::TakeAddr(child) => {
            let child_node = &child.upgrade().unwrap();
            let s = convert_body(child_node, parent, context, i, err_collector)?;
            Some(SHIR::TakeAddr(Box::new(s)))
        }
        Expression::DataType(_) => None,
        Expression::TypeCast(n, t) => {
            let child_node = &unsafe { n.as_ptr().as_ref()? };
            let mut s = convert_body(child_node, parent, context, i, err_collector)?;
            s.type_cast(t.into_basic_type(&context.symbols)?);
            Some(s)
        }
        Expression::TypeDef(name, rhs_type) => {
            context
                .symbols
                .local
                .insert(Rc::clone(name), Symbol::TypeName(rhs_type.clone()));
            None
        }
        Expression::Block(_) => None,
        Expression::Loop(_) => todo!(),
        Expression::Return(child_node) => {
            if let Some(node) = child_node {
                let node = unsafe { node.as_ptr().as_ref()? };
                if let Some(expected_ret_type) = &context.parent_fn_ret_type {
                    check_type(expected_ret_type, node, err_collector, &context.symbols);
                }
                let s = convert_body(node, parent, context, i, err_collector)?;
                Some(SHIR::ReturnValue(Box::new(s)))
            } else {
                if context.parent_fn_ret_type != Some(TypeExpr::none) {
                    err_collector.errors.push(CompileError {
                        content: ErrorContent::MismatchedType {
                            expected: context
                                .parent_fn_ret_type
                                .as_ref()
                                .unwrap_or(&TypeExpr::none)
                                .clone(),
                            found: Some(TypeExpr::none),
                        },
                        position: node.pos,
                        length: "return".len(),
                    })
                }
                Some(SHIR::ReturnVoid)
            }
        }
        Expression::UnsafeReturn => Some(SHIR::ReturnVoid),
        Expression::Break => todo!(),
        Expression::Continue => todo!(),
        Expression::Extern(_, _) => panic!(),
        Expression::RawASM(code) => Some(SHIR::RawASM(Rc::clone(code))),
    }
}

/// Check if the function has a return value, if not:
///     - if the function has a return type of `none`, insert an implicit return at the exit
///     - otherwise, generate an error
fn check_return(
    ret_type: &TypeExpr,
    fn_body: &mut Vec<SHIR>,
    fn_def_node: &ASTNode,
    context: &mut Context,
    err_collector: &mut ErrorCollector,
) {
    if !context.has_ret_statement {
        if ret_type.is_none() {
            // auto return
            fn_body.push(SHIR::ReturnVoid);
        } else {
            err_collector.errors.push(CompileError {
                content: ErrorContent::MissingReturn(ret_type.clone()),
                position: fn_def_node.pos,
                length: 1,
            })
        }
    }
}

fn handle_implicit_printf(
    context: &Context,
    err_collector: &mut ErrorCollector,
    node: &ASTNode,
    target: &mut Vec<SHIR>,
    str_id: usize,
) {
    // check if the function `printf` exists and the first argument is a string
    let try_find_symbol = || -> Result<(), CompileError> {
        let (name, symbol) = context
            .symbols
            .global
            .get_key_value(&"printf".to_string())
            .ok_or(CompileError {
                content: ErrorContent::SymbolNotExist(Rc::new("printf".to_string())),
                position: node.pos,
                length: 1,
            })?;
        let (args, _, _) = symbol.as_function().ok_or(CompileError {
            content: ErrorContent::NotAFunc(Rc::clone(name)),
            position: node.pos,
            length: 1,
        })?;
        let first_arg_type = args.first().ok_or(CompileError {
                            content: ErrorContent::Raw("The function `printf` doesn't have any arguments, and thus it cannot be implicitly called".to_string()),
                            position: node.pos,
                            length: 1,
                        })?;
        if !first_arg_type.is_equvalent(&TypeExpr::Ptr(Box::new(TypeExpr::u8)), &context.symbols) {
            Err(CompileError {
                                content: ErrorContent::Raw("The first argument of the function `printf` is not of type *u8, and thus it cannot be implicitly called".to_string()),
                                position: node.pos,
                                length: 1,
                        })
        } else {
            target.push(SHIR::FnCall {
                name: Rc::clone(name),
                args: vec![(SHIR::Const(SHIRConst::String(str_id)), BasicType::Pointer)],
                ret_type: BasicType::Irrelavent,
            });
            Ok(())
        }
    }();
    if let Err(e) = try_find_symbol {
        err_collector.errors.push(e);
    }
}
