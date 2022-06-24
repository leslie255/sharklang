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
    #[allow(unused)]
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
    #[allow(unused)]
    pub fn matches(
        &self,
        ast: &AST,
        expr: &Expression,
        var_types: &HashMap<String, DataType>,
    ) -> bool {
        println!("{}:{}\t{:?}", file!(), line!(), var_types);
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
                if let Some(var_type) = var_types.get(var_name) {
                    var_type == self
                } else {
                    false
                }
            }
            _ => false,
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
                        Expression::FuncCall(func_name, args) => {
                            if let Some(i) = ast.func_defs.get(func_name) {
                                let block = match ast.expr(*i) {
                                    Expression::FuncDef(_, b) => b,
                                    _ => panic!(),
                                };
                                if block.args.len() != args.len() {
                                    err_count += 1;
                                    println!(
                                        "\texpected {} arguments for function {}, found {}",
                                        block.args.len(),
                                        func_name,
                                        args.len()
                                    );
                                }
                                // TODO: check if function arguments have the correct types
                            } else if builtin_fns.is_builtin_func(func_name) {
                                // TODO: argument check for builtin functions
                            } else {
                                err_count += 1;
                                println!("{} is not the name of a function", func_name);
                            }
                        }
                        Expression::VarAssign(var_name, _) => {
                            // TODO: check if rhs matches the type of lhs
                            if !block.var_addrs.contains_key(var_name) {
                                err_count += 1;
                                println!("{} is not a variable", var_name)
                            }
                        }
                        _ => (),
                    }
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
