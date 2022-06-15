use crate::codegen::ast::*;
use crate::codegen::ir::*;

use std::collections::HashMap;

#[allow(unused_macros)]
macro_rules! print_ast {
    ($ast: expr) => {
        for (i, node) in $ast.iter().enumerate() {
            println!("{}: {:?}", i, node);
        }
    };
}

fn add_builtin_fn_if_needed(
    name: &str,
    externs: &mut Vec<ASMStatement>,
    data_sect: &mut Vec<ASMStatement>,
    text_sect: &mut Vec<ASMStatement>,
    existed: &mut HashMap<&str, bool>,
) {
    match name {
        "print" | "print_int" => {
            if existed.contains_key("print") {
                return;
            }
            externs.push(asm!(extern, "printf"));
            data_sect.push(asm!(data_str, "printint_fmt", "%d\n"));
            existed.insert("print", true);
        }
        "add" => {
            if existed.contains_key("add") {
                return;
            }
            existed.insert("add", true);

            text_sect.push(asm!(func_def, "add"));
            text_sect.push(asm!(mov, rax!(), rdi!()));
            text_sect.push(asm!(add, rax!(), rsi!()));
            text_sect.push(asm!(func_ret));
        }
        "sub" => {
            if existed.contains_key("sub") {
                return;
            }
            existed.insert("sub", true);

            text_sect.push(asm!(func_def, "sub"));
            text_sect.push(asm!(mov, rax!(), rdi!()));
            text_sect.push(asm!(sub, rax!(), rsi!()));
            text_sect.push(asm!(func_ret));
        }
        "mul" => {
            if existed.contains_key("mul") {
                return;
            }
            existed.insert("mul", true);

            text_sect.push(asm!(func_def, "mul"));
            text_sect.push(asm!(mov, rax!(), rdi!()));
            text_sect.push(asm!(mul, rsi!()));
            text_sect.push(asm!(func_ret));
        }
        "div" => {
            if existed.contains_key("div") {
                return;
            }
            existed.insert("div", true);

            text_sect.push(asm!(func_def, "div"));
            text_sect.push(asm!(mov, rax!(), rdi!()));
            text_sect.push(asm!(div, rsi!()));
            text_sect.push(asm!(func_ret));
        }
        _ => {}
    }
}

pub fn codegen(source: String) -> String {
    let raw_ast = construct_ast(source);
    let mut ast = AST::new();
    flatten_ast(&raw_ast, &mut raw_ast.iter(), &mut ast);

    let mut existing_builtin_funcs: HashMap<&str, bool> = HashMap::new();
    let mut str_literals: HashMap<String, String> = HashMap::new();

    // first generate data sections, add needed externs, and generate builtin functions in text
    // section
    let mut target_externs: Vec<ASMStatement> = Vec::new();
    let mut target_data_sect: Vec<ASMStatement> = vec![asm!(sect, "data")];
    let mut target_text_sect: Vec<ASMStatement> = vec![asm!(sect, "text")];
    for node in &ast {
        match &node.expr {
            Expression::StringLiteral(str) => {
                let id = format!("strliteral{}", quick_rand(str));
                str_literals.insert(str.clone(), id.clone());
                target_data_sect.push(asm!(data_str, id.clone(), str));
            }
            Expression::VarInitFunc(var_name, func_name, _) => {
                let asm = asm!(data_int, var_name, 0);
                target_data_sect.push(asm);
                add_builtin_fn_if_needed(
                    func_name.as_str(),
                    &mut target_externs,
                    &mut target_data_sect,
                    &mut target_text_sect,
                    &mut existing_builtin_funcs,
                );
            }
            Expression::FuncCall(name, _) => add_builtin_fn_if_needed(
                name.as_str(),
                &mut target_externs,
                &mut target_data_sect,
                &mut target_text_sect,
                &mut existing_builtin_funcs,
            ),
            Expression::VarInit(name, rhs) => {
                let asm = match &ast.get(*rhs).unwrap().expr {
                    Expression::NumberLiteral(num) => asm!(data_int, name, *num),
                    Expression::Identifier(_) => asm!(data_int, name, 0),
                    _ => panic!("Invalid rhs for `let`"),
                };
                target_data_sect.push(asm);
            }
            _ => {}
        }
    }

    // then generate text section
    target_text_sect.push(asm!(func_def, "main"));
    for node in &ast {
        match &node.expr {
            Expression::FuncCall(name, args) => {
                let mut func_call: ASMFuncCallConstructor =
                    if name == "print" || name == "print_int" {
                        asm!(func_call, &"printf")
                    } else {
                        asm!(func_call, &"printf")
                    };
                if name == "print_int" {
                    func_call.arg_ptr(&Expression::Identifier("printint_fmt".to_string()));
                }
                args.iter().for_each(|arg_i| {
                    let arg = &ast.get(*arg_i).unwrap().expr;
                    func_call.arg_val(arg);
                });
                target_text_sect.push(func_call.asm);
            }
            Expression::VarInitFunc(var_name, func_name, args) => {
                let mut func_call = asm!(func_call, func_name);
                args.iter().for_each(|arg_i| {
                    let arg = &ast.get(*arg_i).unwrap().expr;
                    func_call.arg_val(arg);
                });
                target_text_sect.push(func_call.asm);
                let var_set = asm!(mov, Operand::Var(var_name.clone()), rax!());
                target_text_sect.push(var_set);
            }
            Expression::VarSet(lhs, rhs) => match &ast.get(*rhs).unwrap().expr {
                Expression::NumberLiteral(num) => {
                    target_text_sect.push(asm!(mov, Operand::Var(lhs.clone()), Operand::Int(*num)));
                }
                _ => panic!(
                    "unexpected expression {:?} as the rhs of `set`",
                    ast.get(*rhs).unwrap().expr
                ),
            },
            Expression::VarInit(lhs, rhs) => match &ast.get(*rhs).unwrap().expr {
                Expression::Identifier(val) => {
                    target_text_sect.push(asm!(
                        mov,
                        rax!(),
                        Operand::Var(val.clone())
                    ));
                    target_text_sect.push(asm!(
                        mov,
                        Operand::Var(lhs.clone()),
                        rax!()
                    ));
                }
                _ => {}
            },
            _ => {}
        }
    }
    target_text_sect.push(asm!(func_ret, Operand::Int(0)));

    let mut full_generated_code = String::new();
    for statement in &target_externs {
        full_generated_code.push_str(statement.gen_code().as_str());
    }
    for statement in &target_data_sect {
        full_generated_code.push_str(statement.gen_code().as_str());
    }
    for statement in &target_text_sect {
        full_generated_code.push_str(statement.gen_code().as_str());
    }
    full_generated_code
}
