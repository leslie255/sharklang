use super::ast::*;
use super::builtin_funcs::*;
use super::ir::*;
use super::tokens::*;
use super::preprocess::*;

pub fn codegen(source: String) -> String {
    let preprocessed = preprocess(source);
    let tokens = parse_tokens(preprocessed);
    let ast = construct_ast(tokens);

    let mut program = Program::new();
    let mut builtin_fns = BuiltinFuncChecker::new();
    let mut main_func: Vec<ASMStatement> = vec![asm!(func_def, "main")];

    for node in &ast.nodes {
        if let Expression::FuncCall(name, _) = &node.expr {
            builtin_fns.add_if_needed(name, &mut program);
        }
    }

    for node in &ast.nodes {
        match &node.expr {
            Expression::Identifier(_) => {}
            Expression::NumberLiteral(_) => {}
            Expression::StringLiteral(_) => {}
            Expression::RawASM(text) => main_func.push(asm!(format!("\t{}", text))),
            Expression::Label(name) => main_func.push(asm!(label, name)),
            Expression::VarInit(var_name, rhs) => {
                match ast.expr(*rhs) {
                    Expression::Identifier(id) => {
                        program.data_sect.push(asm!(data_int, var_name, 0));
                        main_func.push(asm!(mov, rax!(), Operand::Var(id.clone())));
                        main_func.push(asm!(mov, Operand::Var(var_name.clone()), rax!()));
                    }
                    Expression::NumberLiteral(num) => {
                        program.data_sect.push(asm!(data_int, var_name, *num));
                    }
                    Expression::StringLiteral(str) => {
                        program.data_sect.push(asm!(data_str, var_name, str));
                    }
                    Expression::FuncCall(_, _) => {
                        program.data_sect.push(asm!(data_int, var_name, 0));
                        // there is no need to call the function because it has been called before
                        main_func.push(asm!(mov, Operand::Var(var_name.clone()), rax!()));
                    }
                    _ => todo!(),
                };
            }
            Expression::VarAssign(var_name, rhs) => match ast.expr(*rhs) {
                Expression::Identifier(id) => {
                    main_func.push(asm!(mov, rax!(), Operand::Var(id.clone())));
                    main_func.push(asm!(mov, Operand::Var(var_name.clone()), rax!()));
                }
                Expression::NumberLiteral(num) => {
                    main_func.push(asm!(
                        mov,
                        Operand::Var(var_name.clone()),
                        Operand::Int(*num)
                    ));
                }
                Expression::StringLiteral(_) => {
                    println!("using string literal as the rhs of variable assignment has not been implemented yet");
                    todo!();
                }
                Expression::FuncCall(func_name, args) => {
                    let mut func_call = asm!(func_call, func_name);
                    for arg in args {
                        match ast.expr(*arg) {
                            Expression::Identifier(id) => {
                                func_call.arg(Operand::Var(id.clone()));
                            }
                            Expression::NumberLiteral(num) => {
                                func_call.arg(Operand::Int(*num));
                            }
                            Expression::StringLiteral(_) => {
                                todo!();
                            }
                            _ => panic!(""),
                        }
                    }
                    main_func.push(func_call.asm);
                    main_func.push(asm!(mov, Operand::Var(var_name.clone()), rax!()));
                }
                _ => todo!(),
            },
            Expression::FuncCall(func_name, args) => {
                let mut func_call = asm!(func_call, func_name);
                for arg in args {
                    match ast.expr(*arg) {
                        Expression::Identifier(id) => {
                            func_call.arg(Operand::Var(id.clone()));
                        }
                        Expression::NumberLiteral(num) => {
                            func_call.arg(Operand::Int(*num));
                        }
                        Expression::StringLiteral(_) => {
                            todo!();
                        }
                        Expression::FuncCall(_, _) => {
                            todo!();
                        }
                        _ => panic!("{:?} is not a valid argument for function", ast.expr(*arg)),
                    }
                }
                main_func.push(func_call.asm);
            }
            _ => todo!(),
        }
    }
    main_func.push(asm!(func_ret, Operand::Int(0)));
    program.funcs.push(main_func);

    program.gen_code()
}
