use super::ast::*;
use super::builtin_funcs::*;
use super::ir::*;
use super::tokens::*;

pub fn recursive_handle_func_call(tree: &AST, expr: &Expression, asm: &mut Vec<ASMStatement>) {
    if let Expression::FuncCall(name, args) = expr {
        let mut func_call = asm!(func_call, name);
        args.iter().for_each(|i| {
            let arg = tree.expr(*i);
            match arg {
                Expression::NumberLiteral(num) => {
                    func_call.arg(Operand::Int(*num));
                }
                Expression::Identifier(id) => {
                    func_call.arg(Operand::Var(id.clone()));
                }
                Expression::FuncCall(_, _) => {
                    todo!();
                }
                _ => {}
            }
        });
        asm.push(func_call.asm);
    }
}

#[allow(unused)]
pub fn codegen(source: String) -> String {
    let tokens = parse_tokens(source);
    let ast = construct_ast(&tokens);

    let mut program = Program::new();
    let mut builtin_fns = BuiltinFuncChecker::new();
    let mut main_func: Vec<ASMStatement> = vec![asm!(func_def, "main")];

    for i in &ast.root_nodes {
        match ast.expr(*i) {
            Expression::VarInit(var_name, rhs) => {
                let asm = match ast.expr(*rhs) {
                    Expression::NumberLiteral(num) => {
                        program.data_sect.push(asm!(data_int, var_name, *num))
                    }
                    Expression::StringLiteral(str) => {
                        program.data_sect.push(asm!(data_str, var_name, str))
                    }
                    Expression::FuncCall(func_name, args) => {
                        builtin_fns.add_if_needed(func_name, &mut program);
                        recursive_handle_func_call(&ast, &ast.expr(*i), &mut main_func);
                        main_func.push(asm!(mov, Operand::Var("temp".to_string()), rax!()));
                    }
                    _ => todo!(),
                };
            }
            Expression::FuncCall(func_name, args) => {
                builtin_fns.add_if_needed(func_name, &mut program);
                recursive_handle_func_call(&ast, &ast.expr(*i), &mut main_func);
            }
            _ => {}
        }
    }
    main_func.push(asm!(func_ret, Operand::Int(0)));
    program.funcs.push(main_func);

    program.gen_code()
}
