use super::ast::*;
use super::builtin_funcs::*;

use std::collections::HashMap;

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
}

impl DataType {
    pub fn from_str(str: String) -> Option<DataType> {
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
            _ => None,
        }
    }
    pub fn size(&self) -> u64 {
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
        }
    }
    #[allow(unused_variables)]
    pub fn matches(
        &self,
        ast: &AST,
        expr: &Expression,
        var_infos: &HashMap<String, VarInfo>,
    ) -> bool {
        match expr {
            Expression::NumberLiteral(_) => match self {
                DataType::UInt8 | DataType::UInt64 | DataType::UInt32 | DataType::UInt16 => true,
                _ => false,
            },
            Expression::FuncCall(name, _) => {
                println!("type check is not implemented for function return values, assuming that the function `{}` returns value of type `{}`", name, self.description());
                true
            }
            Expression::Identifier(var_name) => {
                if let Some(var_info) = var_infos.get(var_name) {
                    &var_info.data_type == self
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

fn var_exist_check(err_count: &mut usize, var_name: &String, block: &CodeBlock, i: usize) -> bool {
    if let Some(var_info) = block.vars.get(var_name) {
        if var_info.def_i > i {
            *err_count += 1;
            println!("using the variable `{}` before it was declared", var_name);
            return false;
        }
        true
    } else {
        *err_count += 1;
        println!("{} is not a variable", var_name);
        false
    }
}

fn fn_exist_check(
    err_count: &mut usize,
    fn_name: &String,
    ast: &AST,
    builtin_fns: &BuiltinFuncChecker,
) -> bool {
    if ast.func_defs.contains_key(fn_name) {
        true
    } else if builtin_fns.is_builtin_func(fn_name) {
        true
    } else {
        *err_count += 1;
        println!("`{}` is not a function", fn_name);
        false
    }
}

pub fn fn_args_check(err_count: &mut usize, ast: &AST, fn_name: &String, input_args: &Vec<usize>) {
    if !ast.func_defs.contains_key(fn_name) {
        // TODO: arguments check for builtin functions
        return;
    }
    if let Expression::FuncDef(_, block) = ast.expr(*ast.func_defs.get(fn_name).unwrap()) {
        if input_args.len() != block.arg_types.len() {
            *err_count += 1;
            println!(
                "expected needs {} arguments for function `{}`, found {}",
                block.arg_types.len(),
                fn_name,
                input_args.len()
            );
        }
        for (i, arg_i) in input_args.iter().enumerate() {
            let arg = ast.expr(*arg_i);
            if let Expression::Identifier(id) = arg {
                if !var_exist_check(err_count, id, block, *arg_i) {
                    continue;
                }
            }
            let expected_type = block.arg_types.get(i).unwrap();
            if !expected_type.matches(ast, arg, &block.vars) {
                *err_count += 1;
                println!(
                    "expected expression of type {} for argument of function `{}`, found {:?}",
                    expected_type.description(),
                    fn_name,
                    arg
                );
            }
        }
    }
}

pub fn type_check(ast: &AST, builtin_fns: &BuiltinFuncChecker) -> usize {
    // returns the number of errors

    let mut err_count: usize = 0;

    for node in &ast.nodes {
        match &node.expr {
            Expression::FuncDef(_, block) => {
                for i in &block.body {
                    let node = ast.node(*i);
                    match &node.expr {
                        Expression::FuncCall(fn_name, args) => {
                            if fn_exist_check(&mut err_count, fn_name, ast, builtin_fns) {
                                fn_args_check(&mut err_count, ast, fn_name, args);
                            }
                        }
                        Expression::VarAssign(var_name, rhs) => {
                            let rhs_expr = ast.expr(*rhs);
                            if !var_exist_check(&mut err_count, var_name, block, *i) {
                                continue;
                            }
                            if let Expression::FuncCall(fn_name, args) = rhs_expr {
                                fn_exist_check(&mut err_count, fn_name, ast, builtin_fns);
                                fn_args_check(&mut err_count, ast, fn_name, args);
                            }
                            let lhs_type = &block.vars.get(var_name).unwrap().data_type;
                            if !lhs_type.matches(ast, rhs_expr, &block.vars) {
                                err_count += 1;
                                println!(
                                    "expecting expression of type `{}` as rhs, found {:?}",
                                    lhs_type.description(),
                                    rhs_expr
                                );
                            }
                        }
                        Expression::VarInit(_, var_type, rhs) => {
                            let rhs_expr = ast.expr(*rhs);
                            if let Expression::Identifier(id) = rhs_expr {
                                var_exist_check(&mut err_count, id, block, *i);
                            } else if let Expression::FuncCall(fn_name, args) = rhs_expr {
                                fn_exist_check(&mut err_count, fn_name, ast, builtin_fns);
                                fn_args_check(&mut err_count, ast, fn_name, args);
                            }
                            if !var_type.matches(ast, rhs_expr, &block.vars) {
                                err_count += 1;
                                println!(
                                    "expecting expression of type `{}` as rhs, found {:?}",
                                    var_type.description(),
                                    rhs_expr
                                );
                            }
                        }
                        _ => (),
                    }
                }
            }
            Expression::VarAssign(_, _) => {
                if node.is_top_level {
                    err_count += 1;
                    println!("variable assignment is not allowed at top level");
                    continue;
                }
            }
            Expression::FuncCall(_, _) => {
                if node.is_top_level {
                    err_count += 1;
                    println!("function calling is not allowed at top level");
                    continue;
                }
            }
            _ => (),
        }
    }

    err_count
}
