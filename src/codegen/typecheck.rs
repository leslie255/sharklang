use super::ast::*;
use super::builtin_funcs::*;
use super::error::*;

#[allow(unused)]
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum DataType {
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,
    Pointer,
    Void,
}

impl DataType {
    pub fn from_str(str: &String) -> Option<DataType> {
        match str.as_str() {
            "uint8" => Some(DataType::UInt8),
            "uint16" => Some(DataType::UInt16),
            "uint32" => Some(DataType::UInt32),
            "uint64" => Some(DataType::UInt64),
            "int8" => Some(DataType::Int8),
            "int16" => Some(DataType::Int16),
            "int32" => Some(DataType::Int32),
            "int64" => Some(DataType::Int64),
            "float32" => Some(DataType::Float32),
            "float64" => Some(DataType::Float64),
            "ptr" => Some(DataType::Pointer),
            "void" => Some(DataType::Pointer),
            _ => None,
        }
    }
    pub fn size(&self) -> u64 {
        // in bytes not bits
        match self {
            DataType::UInt8 => 1,
            DataType::UInt16 => 2,
            DataType::UInt32 => 4,
            DataType::UInt64 => 8,
            DataType::Int8 => 1,
            DataType::Int16 => 2,
            DataType::Int32 => 4,
            DataType::Int64 => 8,
            DataType::Float32 => 1,
            DataType::Float64 => 2,
            DataType::Pointer => 8,
            DataType::Void => 0,
        }
    }
    pub fn description(&self) -> String {
        match self {
            DataType::UInt8 => String::from("uint8"),
            DataType::UInt16 => String::from("uint16"),
            DataType::UInt32 => String::from("uint32"),
            DataType::UInt64 => String::from("uint64"),
            DataType::Int8 => String::from("int8"),
            DataType::Int16 => String::from("int16"),
            DataType::Int32 => String::from("int32"),
            DataType::Int64 => String::from("int64"),
            DataType::Float32 => String::from("float32"),
            DataType::Float64 => String::from("float64"),
            DataType::Pointer => String::from("ptr"),
            DataType::Void => String::from("void"),
        }
    }
    pub fn matches(&self, context: &TypeCheckContext, expr: &Expression) -> bool {
        match expr {
            Expression::NumberLiteral(_) => match self {
                DataType::UInt8
                | DataType::UInt64
                | DataType::UInt32
                | DataType::UInt16
                | DataType::Int8
                | DataType::Int64
                | DataType::Int32
                | DataType::Int16 => true,
                _ => false,
            },
            Expression::StringLiteral(_) => self == &DataType::Pointer,
            Expression::FuncCall(fn_name, _) => {
                if let Some(i) = context.ast.func_defs.get(fn_name) {
                    if let Expression::FuncDef(_, fn_block) = context.ast.expr(*i) {
                        return self == &fn_block.return_type;
                    } else {
                        return false;
                    }
                } else {
                    // is a builtin function
                    return true;
                };
            }
            Expression::Identifier(var_name) => {
                if let Some(var_info) = context.parent_block.vars.get(var_name) {
                    &var_info.data_type == self
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

pub struct TypeCheckContext<'a> {
    pub ast: &'a AST,
    pub parent_block: &'a CodeBlock,
    pub builtin_fns: &'a BuiltinFuncChecker,
    pub i: usize,
}
impl<'a> TypeCheckContext<'a> {
    fn new(
        ast: &'a AST,
        parent_block: &'a CodeBlock,
        builtin_fns: &'a BuiltinFuncChecker,
        i: usize,
    ) -> TypeCheckContext<'a> {
        TypeCheckContext {
            ast,
            parent_block,
            builtin_fns,
            i,
        }
    }
}

fn var_exist_check(
    context: &mut TypeCheckContext,
    err_collector: &mut ErrorCollector,
    var_name: &String,
) -> bool {
    if let Some(var_info) = context.parent_block.vars.get(var_name) {
        if var_info.def_i > context.i {
            err_collector.errors.push(CompileError {
                err_type: ErrorType::Type,
                message: format!("using the variable `{}` before it was declared", var_name),
                position: context.ast.node(context.i).position,
                length: usize::MAX,
            });
            return false;
        }
        true
    } else {
        err_collector.errors.push(CompileError {
            err_type: ErrorType::Type,
            message: format!("`{}` is not a variable", var_name),
            position: context.ast.node(context.i).position,
            length: usize::MAX,
        });
        return false;
    }
}

fn fn_exist_check(
    context: &mut TypeCheckContext,
    err_collector: &mut ErrorCollector,
    fn_name: &String,
) -> bool {
    if context.ast.func_defs.contains_key(fn_name) {
        true
    } else if context.builtin_fns.is_builtin_func(fn_name) {
        true
    } else {
        err_collector.errors.push(CompileError {
            err_type: ErrorType::Type,
            message: format!("`{}` is not a function", fn_name),
            position: context.ast.node(context.i).position,
            length: usize::MAX,
        });
        false
    }
}

pub fn fn_args_check(
    context: &mut TypeCheckContext,
    err_collector: &mut ErrorCollector,
    fn_name: &String,
    input_args: &Vec<usize>,
) {
    if !context.ast.func_defs.contains_key(fn_name) {
        // is a builtin function
        // TODO: argument count and type check for builtin functions
        // currently only check if variables used exists
        for arg_i in input_args {
            let arg = context.ast.expr(*arg_i);
            if let Expression::Identifier(id) = arg {
                if !var_exist_check(context, err_collector, id) {
                    continue;
                }
            }
        }
        return;
    }
    if let Expression::FuncDef(_, fn_block) = context
        .ast
        .expr(*context.ast.func_defs.get(fn_name).unwrap())
    {
        // check number of arguments
        if input_args.len() != fn_block.arg_types.len() {
            err_collector.errors.push(CompileError {
                err_type: ErrorType::Type,
                message: format!(
                    "function `{}` has {} arguments, provided {}",
                    fn_name,
                    fn_block.arg_types.len(),
                    input_args.len()
                ),
                position: context.ast.node(context.i).position,
                length: usize::MAX,
            });
            return;
        }

        // check arguments
        for (i, arg_i) in input_args.iter().enumerate() {
            let arg = context.ast.expr(*arg_i);
            if let Expression::Identifier(id) = arg {
                if !var_exist_check(context, err_collector, id) {
                    continue;
                }
            }
            let expected_type = fn_block.arg_types.get(i).unwrap();
            if !expected_type.matches(context, arg) {
                err_collector.errors.push(CompileError {
                    err_type: ErrorType::Type,
                    message: format!(
                        "expected expression of type {} for argument of function `{}`, found {:?}",
                        expected_type.description(),
                        fn_name,
                        arg
                    ),
                    position: context.ast.node(context.i).position,
                    length: usize::MAX,
                });
            }
        }
    }
}

pub fn type_check(ast: &AST, builtin_fns: &BuiltinFuncChecker, err_collector: &mut ErrorCollector) {
    for node in &ast.nodes {
        match &node.expr {
            Expression::FuncDef(_, block) => {
                for i in &block.body {
                    let node = ast.node(*i);
                    let mut context = TypeCheckContext::new(ast, block, builtin_fns, *i);
                    match &node.expr {
                        Expression::FuncCall(fn_name, args) => {
                            if fn_exist_check(&mut context, err_collector, fn_name) {
                                fn_args_check(&mut context, err_collector, fn_name, args);
                            }
                        }
                        Expression::VarAssign(var_name, rhs) => {
                            let rhs_expr = ast.expr(*rhs);
                            if !var_exist_check(&mut context, err_collector, var_name) {
                                continue;
                            }
                            if let Expression::FuncCall(fn_name, args) = rhs_expr {
                                if fn_exist_check(&mut context, err_collector, fn_name) {
                                    fn_args_check(&mut context, err_collector, fn_name, args);
                                }
                            }
                            let lhs_type = &block.vars.get(var_name).unwrap().data_type;
                            if !lhs_type.matches(&context, rhs_expr) {
                                err_collector.errors.push(CompileError {
                                    err_type: ErrorType::Type,
                                    message: format!(
                                        "expecting expression of type `{}` as rhs",
                                        lhs_type.description()
                                    ),
                                    position: ast.node(*rhs).position,
                                    length: usize::MAX,
                                });
                            }
                        }
                        Expression::VarInit(_, var_type, rhs) => {
                            let rhs_expr = ast.expr(*rhs);
                            if let Expression::Identifier(id) = rhs_expr {
                                var_exist_check(&mut context, err_collector, id);
                            } else if let Expression::FuncCall(fn_name, args) = rhs_expr {
                                if fn_exist_check(&mut context, err_collector, fn_name) {
                                    fn_args_check(&mut context, err_collector, fn_name, args);
                                }
                            }
                            if !var_type.matches(&context, rhs_expr) {
                                err_collector.errors.push(CompileError {
                                    err_type: ErrorType::Type,
                                    message: format!(
                                        "expecting expression of type `{}` as rhs",
                                        var_type.description()
                                    ),
                                    position: ast.node(*rhs).position,
                                    length: usize::MAX,
                                });
                            }
                        }
                        Expression::ReturnVal(i) => {
                            let expr = ast.expr(*i);
                            match expr {
                                Expression::Identifier(var_name) => {
                                    var_exist_check(&mut context, err_collector, var_name);
                                }
                                Expression::FuncCall(fn_name, args) => {
                                    fn_exist_check(&mut context, err_collector, fn_name);
                                    fn_args_check(&mut context, err_collector, fn_name, args);
                                }
                                _ => (),
                            }
                            if !block.return_type.matches(&context, expr) {
                                err_collector.errors.push(CompileError {
                                    err_type: ErrorType::Type,
                                    message: format!(
                                        "expecting expression of type `{}` after `return`",
                                        block.return_type.description()
                                    ),
                                    position: ast.node(*i).position,
                                    length: usize::MAX,
                                });
                            }
                        }
                        Expression::ReturnVoid => {
                            if block.return_type != DataType::Void {
                                err_collector.errors.push(CompileError {
                                    err_type: ErrorType::Type,
                                    message: format!("this function does not have a return type"),
                                    position: ast.node(*i).position,
                                    length: usize::MAX,
                                });
                            }
                        }
                        _ => (),
                    }
                }
            }
            Expression::VarAssign(_, _) => {
                if node.is_top_level {
                    err_collector.errors.push(CompileError {
                        err_type: ErrorType::Type,
                        message: format!("variable assignment is not allowed at top level"),
                        position: node.position,
                        length: usize::MAX,
                    });
                    continue;
                }
            }
            Expression::FuncCall(_, _) => {
                if node.is_top_level {
                    err_collector.errors.push(CompileError {
                        err_type: ErrorType::Type,
                        message: format!("function calling is not allowed at top level"),
                        position: node.position,
                        length: usize::MAX,
                    });
                    continue;
                }
            }
            _ => (),
        }
    }
}
