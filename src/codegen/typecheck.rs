use super::ast::*;
use super::builtin_funcs::*;
use super::error::*;

use std::collections::HashSet;

#[allow(unused)]
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
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
        if let Expression::FuncCall(fn_name, _) = expr {
            if context.builtin_fns.is_builtin_func(fn_name) {
                return true;
            }
        }
        DataType::possible_types(context, expr).contains(self)
    }
    pub fn possible_types(context: &TypeCheckContext, expr: &Expression) -> HashSet<DataType> {
        let mut hash_set: HashSet<DataType> = HashSet::new();
        match expr {
            Expression::NumberLiteral(_) => {
                hash_set.insert(DataType::UInt8);
                hash_set.insert(DataType::UInt64);
                hash_set.insert(DataType::UInt32);
                hash_set.insert(DataType::UInt16);
                hash_set.insert(DataType::Int8);
                hash_set.insert(DataType::Int64);
                hash_set.insert(DataType::Int32);
                hash_set.insert(DataType::Int16);
            }
            Expression::StringLiteral(_) => {
                hash_set.insert(DataType::Pointer);
            }
            Expression::CharLiteral(_) => {
                hash_set.insert(DataType::UInt8);
            }
            Expression::FuncCall(fn_name, _) => {
                if let Some(fn_block) = context.ast.fn_block(fn_name) {
                    hash_set.insert(fn_block.return_type.clone());
                }
            }
            Expression::Identifier(var_name) => {
                if let Some(var_info) = context.parent_block.var_info(var_name, context.ast) {
                    hash_set.insert(var_info.data_type.clone());
                }
            }
            Expression::TypeCast(_, data_type) => {
                hash_set.insert(data_type.clone());
            }
            _ => (),
        }
        hash_set
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
    if let Some(var_info) = context.parent_block.var_info(var_name, context.ast) {
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
            message: format!("variable `{}` is not found in the current scope", var_name),
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

fn fn_arg_check(
    // check one of the arguments
    context: &mut TypeCheckContext,
    err_collector: &mut ErrorCollector,
    expected_args: &Vec<DataType>,
    arg: usize, // the argument provided
    i: usize,   // which argument is this?
    check_type: bool,
) -> bool {
    // first check if variable or function call used as argument is valid
    let arg_expr = context.ast.expr(arg);
    match arg_expr {
        Expression::NumberLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::CharLiteral(_) => (),
        Expression::Identifier(id) => {
            if !var_exist_check(context, err_collector, id) {
                return false;
            }
        }
        Expression::FuncCall(fn_name, args) => {
            if !fn_exist_check(context, err_collector, fn_name) {
                return false;
            }
            fn_args_check(context, err_collector, fn_name, args);
        }
        Expression::TypeCast(unwrapped_i, _) => {
            nested_typecast_check(context, err_collector, arg_expr);
            fn_arg_check(
                context,
                err_collector,
                expected_args,
                *unwrapped_i,
                i,
                false,
            );
        }
        _ => {
            err_collector.add_err(
                ErrorType::Syntax,
                context.ast.node(arg).position,
                usize::MAX,
                format!(
                    "{} is not a valid argument for a function",
                    arg_expr.description()
                ),
            );
            return false;
        }
    }

    // check type
    if check_type {
        // Make sure to check the number of arguments before calling this function
        let expected_type = expected_args.get(i).unwrap();
        if !expected_type.matches(context, arg_expr) {
            err_collector.add_err(
                ErrorType::Type,
                context.ast.node(arg).position,
                usize::MAX,
                format!(
                    "expecting expression of type {} for argument #{}",
                    expected_type.description(),
                    i
                ),
            );
            return false;
        }
    }

    // check argument type
    true
}

fn fn_args_check(
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
    if let Some(fn_block) = context.ast.fn_block(fn_name) {
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
            fn_arg_check(context, err_collector, &fn_block.arg_types, *arg_i, i, true);
        }
    }
}

fn nested_typecast_check(
    context: &mut TypeCheckContext,
    err_collector: &mut ErrorCollector,
    type_cast_expr: &Expression,
) -> bool {
    let unwrapped = if let Expression::TypeCast(i, _) = type_cast_expr {
        context.ast.node(*i)
    } else {
        panic!();
    };
    match unwrapped.expr {
        Expression::TypeCast(..) => {
            err_collector.add_err(
                ErrorType::Syntax,
                unwrapped.position,
                usize::MAX,
                format!("Nested type cast is not allowed"),
            );
            false
        }
        _ => true,
    }
}

fn var_init_check(
    context: &mut TypeCheckContext,
    err_collector: &mut ErrorCollector,
    var_name: &String,
    var_type: Option<&DataType>, // if None means skip type check
    rhs_i: usize,
) -> bool {
    // check if lhs variable exists
    if !var_exist_check(context, err_collector, var_name) {
        return false;
    }
    // check if rhs is valid
    let rhs = context.ast.expr(rhs_i);
    match rhs {
        Expression::NumberLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::CharLiteral(_) => (),
        Expression::FuncCall(fn_name, args) => {
            if fn_exist_check(context, err_collector, fn_name) {
                fn_args_check(context, err_collector, fn_name, args);
            } else {
                return false;
            }
        }
        Expression::Identifier(rhs_name) => {
            if !var_exist_check(context, err_collector, rhs_name) {
                return false;
            }
        }
        Expression::TypeCast(i, _) => {
            if !nested_typecast_check(context, err_collector, rhs) {
                return false;
            }
            if !var_init_check(context, err_collector, var_name, None, *i) {
                return false;
            }
        }
        _ => {
            err_collector.add_err(
                ErrorType::Syntax,
                context.ast.node(rhs_i).position,
                usize::MAX,
                format!(
                    "{} is not a valid rhs for variable declareation",
                    rhs.description()
                ),
            );
            return false;
        }
    }

    // check rhs type
    if var_type.is_some() {
        if !var_type.unwrap().matches(context, rhs) {
            err_collector.add_err(
                ErrorType::Syntax,
                context.ast.node(rhs_i).position,
                usize::MAX,
                format!(
                    "expecting expression of type `{}` as rhs",
                    var_type.unwrap().description()
                ),
            );
            return false;
        }
    }
    return true;
}

fn var_assign_check(
    context: &mut TypeCheckContext,
    err_collector: &mut ErrorCollector,
    var_name: &String,
    rhs_i: usize,
    check_type: bool,
) -> bool {
    // check if lhs is a valid variable
    if !var_exist_check(context, err_collector, var_name) {
        return false;
    }
    // check if rhs is a valid expression
    let rhs = context.ast.expr(rhs_i);
    match rhs {
        Expression::NumberLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::CharLiteral(_) => (),
        Expression::FuncCall(fn_name, args) => {
            if fn_exist_check(context, err_collector, fn_name) {
                fn_args_check(context, err_collector, fn_name, args);
            } else {
                return false;
            }
        }
        Expression::Identifier(id) => {
            if !var_exist_check(context, err_collector, id) {
                return false;
            }
        }
        Expression::TypeCast(unwrapped_rhs, _) => {
            if !nested_typecast_check(context, err_collector, rhs) {
                return false;
            }
            if !var_assign_check(context, err_collector, var_name, *unwrapped_rhs, true) {
                return false;
            }
        }
        _ => {
            err_collector.add_err(
                ErrorType::Syntax,
                context.ast.node(rhs_i).position,
                usize::MAX,
                format!(
                    "{} is not a valid rhs for variable assignment",
                    rhs.description()
                ),
            );
        }
    }
    if check_type {
        if !context
            .parent_block
            .var_type(var_name, context.ast)
            .matches(context, rhs)
        {
            err_collector.add_err(
                ErrorType::Type,
                context.ast.node(context.i).position,
                usize::MAX,
                format!(
                    "expecting expression of type `{}` as rhs",
                    context.parent_block.return_type.description()
                ),
            );
            return false;
        }
    }
    true
}

fn return_check(
    context: &mut TypeCheckContext,
    err_collector: &mut ErrorCollector,
    expr: &Expression,
    check_type: bool,
) {
    match expr {
        Expression::NumberLiteral(_) | Expression::StringLiteral(_) => (),
        Expression::FuncCall(fn_name, args) => {
            if fn_exist_check(context, err_collector, &fn_name) {
                fn_args_check(context, err_collector, &fn_name, &args);
            } else {
                return;
            }
        }
        Expression::TypeCast(i, _) => {
            if !nested_typecast_check(context, err_collector, expr) {
                return;
            }
            return_check(context, err_collector, context.ast.expr(*i), false);
        }
        Expression::Identifier(id) => {
            if !var_exist_check(context, err_collector, id) {
                return;
            }
        }
        _ => {
            err_collector.add_err(
                ErrorType::Syntax,
                context.ast.node(context.i).position,
                usize::MAX,
                format!("{} is not a valid return value", expr.description()),
            );
        }
    }
    if check_type {
        if !context.parent_block.return_type.matches(context, expr) {
            err_collector.add_err(
                ErrorType::Type,
                context.ast.node(context.i).position,
                usize::MAX,
                format!(
                    "expecting expression of type `{}` after `return`",
                    context.parent_block.return_type.description()
                ),
            );
        }
    }
}

pub fn type_check(ast: &AST, builtin_fns: &BuiltinFuncChecker, err_collector: &mut ErrorCollector) {
    for node in &ast.nodes {
        match &node.expr {
            Expression::FuncDef(_, block_i) => {
                let block = if let Expression::Block(b) = ast.expr(*block_i) {
                    b
                } else {
                    panic!()
                };
                for i in &block.body {
                    let node = ast.node(*i);
                    let mut context = TypeCheckContext::new(ast, block, builtin_fns, *i);
                    match &node.expr {
                        Expression::FuncCall(fn_name, args) => {
                            if fn_exist_check(&mut context, err_collector, fn_name) {
                                fn_args_check(&mut context, err_collector, fn_name, args);
                            }
                        }
                        Expression::VarAssign(var_name, rhs_i) => {
                            var_assign_check(&mut context, err_collector, var_name, *rhs_i, true);
                        }
                        Expression::VarInit(lhs_name, var_type, rhs_i) => {
                            var_init_check(
                                &mut context,
                                err_collector,
                                lhs_name,
                                Some(var_type),
                                *rhs_i,
                            );
                        }
                        Expression::ReturnVal(i) => {
                            return_check(&mut context, err_collector, ast.expr(*i), true);
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
                        Expression::Loop(i) => {
                            let body = if let Expression::Block(b) = context.ast.expr(*i) {
                                b
                            } else {
                                panic!();
                            };
                            for i in &body.body {
                                if let Expression::VarInit(_, _, _) = context.ast.expr(*i) {
                                    err_collector.errors.push(CompileError {
                                        err_type: ErrorType::Type,
                                        message: format!(
                                            "variable declaration inside loop hasn't been implemented yet"
                                        ),
                                        position: context.ast.node(*i).position,
                                        length: usize::MAX,
                                    });
                                    continue;
                                }
                            }
                        }
                        Expression::FuncDef(..) => {
                            err_collector.add_err(
                                ErrorType::Syntax,
                                ast.node(*i).position,
                                usize::MAX,
                                format!("nested function definition is not allowed"),
                            );
                        }
                        _ => (),
                    }
                }
            }
            Expression::Loop(_) => {
                if node.is_top_level {
                    err_collector.errors.push(CompileError {
                        err_type: ErrorType::Type,
                        message: format!("loop statement is not allowed at top level"),
                        position: node.position,
                        length: usize::MAX,
                    });
                    continue;
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
