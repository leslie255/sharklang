use super::ast::*;
use super::builtin_funcs::*;
use super::error::*;
use super::ir::*;
use super::preprocess::*;
use super::tokens::*;
use super::typecheck::*;

static NESTED_FUNC_CALL_BUFFER_REGS: [Register; 6] = [
    Register::r10,
    Register::r11,
    Register::r12,
    Register::r13,
    Register::r14,
    Register::r15,
];
fn codegen_for_fn_call(
    block: &CodeBlock,
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
                    func_call.arg(Operand::LocalVar(block.var_addr(id).unwrap()));
                }
                Expression::NumberLiteral(num) => {
                    func_call.arg(Operand::Int(*num));
                }
                Expression::StringLiteral(str) => {
                    func_call.arg(Operand::Label(format!(
                        "strliteral_{}",
                        program.strliterals_ids.get(str).unwrap()
                    )));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, ast, ast.node(*arg), target);
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

fn gen_code_inside_block(
    block: &CodeBlock,
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
        Expression::VarInit(var_name, _, rhs) => {
            let addr_lhs = block.var_addr(var_name).unwrap();
            match ast.expr(*rhs) {
                Expression::Identifier(id) => {
                    let addr_rhs = block.var_addr(id).unwrap();
                    target.push(asm!(mov, rax!(), Operand::LocalVar(addr_rhs)));
                    target.push(asm!(mov, Operand::LocalVar(addr_lhs), rax!()));
                }
                Expression::NumberLiteral(num) => {
                    target.push(asm!(mov, Operand::LocalVar(addr_lhs), Operand::Int(*num)));
                }
                Expression::StringLiteral(str) => {
                    let strliteral_id = format!("strliteral_{}", program.strliterals_ids.get(str).unwrap());
                    target.push(asm!(
                        mov,
                        rax!(),
                        Operand::Label(strliteral_id)
                    ));
                    target.push(asm!(
                        mov,
                        Operand::LocalVar(addr_lhs),
                        rax!()
                    ));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, ast.node(*rhs), target);
                    target.push(asm!(mov, Operand::LocalVar(addr_lhs), rax!()));
                }
                _ => {
                    panic!();
                }
            };
        }
        Expression::VarAssign(var_name, rhs) => {
            let addr_lhs = block.var_addr(var_name).unwrap();
            match ast.expr(*rhs) {
                Expression::Identifier(id) => {
                    target.push(asm!(
                        mov,
                        rax!(),
                        Operand::LocalVar(block.var_addr(id).unwrap())
                    ));
                    target.push(asm!(mov, Operand::LocalVar(addr_lhs), rax!()));
                }
                Expression::NumberLiteral(num) => {
                    target.push(asm!(mov, Operand::LocalVar(addr_lhs), Operand::Int(*num)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, &ast.node(*rhs), target);
                    target.push(asm!(mov, Operand::LocalVar(addr_lhs), rax!()));
                }
                _ => panic!("{:?} is not a valid expression", ast.expr(*rhs)),
            }
        }
        Expression::FuncCall(_, _) => {
            if node.is_root {
                codegen_for_fn_call(block, program, &ast, node, target);
            }
        }
        Expression::UnsafeReturn => {
            if block.has_vars {
                target.push(asm!(add, rsp!(), Operand::Int(block.stack_depth)));
            }
            target.push(asm!(func_ret));
        }
        Expression::ReturnVoid => {
            if block.has_vars {
                target.push(asm!(add, rsp!(), Operand::Int(block.stack_depth)));
            }
            target.push(asm!(func_ret));
        }
        Expression::ReturnVal(val) => {
            match ast.expr(*val) {
                Expression::Identifier(id) => {
                    target.push(asm!(
                        mov,
                        rax!(),
                        Operand::LocalVar(block.var_addr(id).unwrap())
                    ));
                }
                Expression::NumberLiteral(num) => {
                    target.push(asm!(mov, rax!(), Operand::Int(*num)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, &ast.node(*val), target);
                }
                _ => panic!("{:?} is not a valid expression", ast.expr(*val)),
            }
            if block.has_vars {
                target.push(asm!(add, rsp!(), Operand::Int(block.stack_depth)));
            }
            target.push(asm!(func_ret));
        }
        _ => {}
    }
}

static ARG_REG_NAMES: [Operand; 6] = [rdi!(), rsi!(), rdx!(), rcx!(), r8!(), r9!()];
pub fn codegen(source: String, src_file: String) -> String {
    let mut err_collector = ErrorCollector::new(src_file, &source);
    let tokens = preprocess(parse_tokens(&source));
    let ast = construct_ast(tokens, &mut err_collector);

    let mut builtin_fns = BuiltinFuncChecker::new();

    type_check(&ast, &builtin_fns, &mut err_collector);
    err_collector.print_errs();

    let mut program = Program::new();

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
            if block.has_vars {
                func.push(asm!(sub, rsp!(), Operand::Int(block.stack_depth)));
            }
            for (i, (_, var_info)) in block.vars.iter().enumerate() {
                if !var_info.is_arg {
                    // arguments always come before other variables
                    break;
                }
                func.push(asm!(
                    mov,
                    Operand::LocalVar(var_info.addr).clone(),
                    ARG_REG_NAMES.get(i)
                        .expect("passing more than 6 arguments into a function haven't been implemented yet")
                        .clone()
                ));
            }
            for i in &block.body {
                gen_code_inside_block(&block, &ast, ast.node(*i), &mut program, &mut func);
            }
            program.funcs.push(func);
        }
        if let Expression::RawASM(code) = &node.expr {
            if node.is_top_level {
                program.data_sect.push(asm!(code));
            }
        }
    }

    if !err_collector.errors.is_empty() {
        err_collector.print_errs();
        println!("could not compile due to errors listed above");
        panic!();
    }
    program.gen_code()
}
