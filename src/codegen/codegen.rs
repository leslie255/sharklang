use super::ast::*;
use super::builtin_funcs::*;
use super::ir::*;
use super::preprocess::*;
use super::tokens::*;

static NESTED_FUNC_CALL_BUFFER_REGS: [Register; 8] = [
    Register::r8,
    Register::r9,
    Register::r10,
    Register::r11,
    Register::r12,
    Register::r13,
    Register::r14,
    Register::r15,
];
fn codegen_for_fn_call(
    program: &Program,
    ast: &AST,
    node: &ASTNode,
    target: &mut Vec<ASMStatement>,
) {
    if let Expression::FuncCall(func_name, args) = &node.expr {
        let mut func_call = asm!(func_call, func_name);
        for (i, arg) in args.iter().enumerate() {
            match ast.expr(*arg) {
                Expression::Identifier(id) => {
                    func_call.arg(Operand::Var(id.clone()));
                }
                Expression::NumberLiteral(num) => {
                    func_call.arg(Operand::Int(*num));
                }
                Expression::StringLiteral(str) => {
                    func_call.arg(Operand::Addr(format!(
                        "strliteral_{}",
                        program.strliterals_ids.get(str).unwrap()
                    )));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(program, ast, ast.node(*arg), target);
                    let reg = NESTED_FUNC_CALL_BUFFER_REGS[i];
                    target.push(asm!(mov, Operand::Reg(reg), rax!()));
                    func_call.arg(Operand::Reg(reg));
                }
                _ => panic!("{:?} is not a valid argument for function", ast.expr(*arg)),
            }
        }
        target.push(func_call.asm);
    }
}

fn gen_code_for_expr(
    ast: &AST,
    node: &ASTNode,
    program: &mut Program,
    target: &mut Vec<ASMStatement>,
) {
    match &node.expr {
        Expression::Identifier(_) => {}
        Expression::NumberLiteral(_) => {}
        Expression::StringLiteral(_) => {}
        Expression::RawASM(text) => target.push(asm!(format!("\t{}", text))),
        Expression::Label(name) => target.push(asm!(label, name)),
        Expression::VarInit(var_name, rhs) => {
            match ast.expr(*rhs) {
                Expression::Identifier(id) => {
                    program.data_sect.push(asm!(data_int, var_name, 0));
                    target.push(asm!(mov, rax!(), Operand::Var(id.clone())));
                    target.push(asm!(mov, Operand::Var(var_name.clone()), rax!()));
                }
                Expression::NumberLiteral(num) => {
                    program.data_sect.push(asm!(data_int, var_name, *num));
                }
                Expression::StringLiteral(str) => {
                    program.data_sect.push(asm!(data_str, var_name, str));
                }
                Expression::FuncCall(_, _) => {
                    program.data_sect.push(asm!(data_int, var_name, 0));
                    codegen_for_fn_call(program, &ast, ast.node(*rhs), target);
                    target.push(asm!(mov, Operand::Var(var_name.clone()), rax!()));
                }
                _ => {
                    println!("using string literal as the rhs of variable declaration has not been implemented yet");
                    todo!();
                }
            };
        }
        Expression::VarAssign(var_name, rhs) => match ast.expr(*rhs) {
            Expression::Identifier(id) => {
                target.push(asm!(mov, rax!(), Operand::Var(id.clone())));
                target.push(asm!(mov, Operand::Var(var_name.clone()), rax!()));
            }
            Expression::NumberLiteral(num) => {
                target.push(asm!(
                    mov,
                    Operand::Var(var_name.clone()),
                    Operand::Int(*num)
                ));
            }
            Expression::FuncCall(_, _) => {
                codegen_for_fn_call(program, &ast, &ast.node(*rhs), target);
                target.push(asm!(mov, Operand::Var(var_name.clone()), rax!()));
            }
            _ => panic!("{:?} is not a valid expression", ast.expr(*rhs)),
        },
        Expression::FuncCall(_, _) => {
            if node.is_root {
                codegen_for_fn_call(program, &ast, node, target);
            }
        }
        Expression::ReturnVoid => target.push(asm!(func_ret)),
        Expression::ReturnVal(val) => {
            match ast.expr(*val) {
                Expression::Identifier(id) => {
                    target.push(asm!(mov, rax!(), Operand::Var(id.clone())));
                }
                Expression::NumberLiteral(num) => {
                    target.push(asm!(mov, rax!(), Operand::Int(*num)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(program, &ast, &ast.node(*val), target);
                }
                _ => panic!("{:?} is not a valid expression", ast.expr(*val)),
            }
            target.push(asm!(func_ret));
        }
        _ => {}
    }
}

pub fn codegen(source: String) -> String {
    let preprocessed = preprocess(source);
    let tokens = parse_tokens(preprocessed);
    let ast = construct_ast(tokens);

    let mut program = Program::new();
    let mut builtin_fns = BuiltinFuncChecker::new();

    // generate string literals
    for (i, node) in ast.nodes.iter().enumerate() {
        if let Expression::StringLiteral(str) = &node.expr {
            program.strliterals_ids.insert(str.clone(), i as u64);
            program
                .data_sect
                .push(asm!(data_str, format!("strliteral_{}", i), str));
        }
    }

    for node in &ast.nodes {
        if let Expression::FuncCall(name, _) = &node.expr {
            builtin_fns.add_if_needed(name, &mut program);
        }
        if let Expression::FuncDef(name, block) = &node.expr {
            let mut func: Vec<ASMStatement> = vec![asm!(func_def, name)];
            for i in &block.body {
                gen_code_for_expr(&ast, ast.node(*i), &mut program, &mut func);
            }
            program.funcs.push(func);
        }
        if let Expression::RawASM(code) = &node.expr {
            if node.is_top_level {
                program.data_sect.push(asm!(code));
            }
        }
    }

    program.gen_code()
}
