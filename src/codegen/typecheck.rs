use super::ast::*;
use super::builtin_funcs::*;

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
    Float64,
    Float32,
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
            "float64" => Some(DataType::Float64),
            "float32" => Some(DataType::Float32),
            "ptr" => Some(DataType::Pointer),
            _ => None,
        }
    }
}

pub fn type_check(ast: &AST, builtin_fns: &BuiltinFuncChecker) -> usize {
    // returns the number of errors

    let mut err_count: usize = 0;

    for node in &ast.nodes {
        match &node.expr {
            Expression::FuncCall(func_name, args) => {
                if let Some(i) = ast.func_defs.get(func_name) {
                    let block = match ast.expr(*i) {
                        Expression::FuncDef(_, b) => b,
                        _ => panic!(),
                    };
                    if block.args.len() != args.len() {
                        err_count += 1;
                        println!("while calling function `{}`:", func_name);
                        println!(
                            "\texpected {} arguments, found {}",
                            block.args.len(),
                            args.len()
                        );
                    }
                    // TODO: check if the type of arguments matches
                } else if builtin_fns.is_builtin_func(func_name) {
                    // TODO: arguments check for builtin functions
                } else {
                    err_count += 1;
                    println!("`{}` is not a function", func_name);
                }
            }
            _ => (),
        }
    }

    err_count
}
