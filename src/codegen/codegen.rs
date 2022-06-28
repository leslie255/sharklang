use super::ast::*;
use super::builtin_funcs::*;
use super::error::*;
use super::ir::*;
use super::preprocess::*;
use super::tokens::*;
use super::typecheck::*;

static NESTED_FUNC_CALL_BUFFER_REGS_8: [Operand; 6] = [
    operand8!(r10),
    operand8!(r11),
    operand8!(r12),
    operand8!(r13),
    operand8!(r14),
    operand8!(r15),
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
                    func_call.arg(operand8!(var, block.var_addr(id).unwrap()));
                }
                Expression::NumberLiteral(num) => {
                    func_call.arg(operand8!(int, *num));
                }
                Expression::StringLiteral(str) => {
                    func_call.arg(operand8!(
                        label,
                        format!("strliteral_{}", program.strliterals_ids.get(str).unwrap())
                    ));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, ast, ast.node(*arg), target);
                    let reg = NESTED_FUNC_CALL_BUFFER_REGS_8.get(i).unwrap();
                    target.push(asm!(mov, reg.clone(), operand8!(rax)));
                    func_call.arg(reg.clone());
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
                    target.push(asm!(mov, operand8!(rax), operand8!(var, addr_rhs)));
                    target.push(asm!(mov, operand8!(var, addr_lhs), operand8!(rax)));
                }
                Expression::NumberLiteral(num) => {
                    target.push(asm!(mov, operand8!(var, addr_lhs), operand8!(int, *num)));
                }
                Expression::StringLiteral(str) => {
                    let strliteral_id =
                        format!("strliteral_{}", program.strliterals_ids.get(str).unwrap());
                    target.push(asm!(mov, operand8!(rax), operand8!(label, strliteral_id)));
                    target.push(asm!(mov, operand8!(var, addr_lhs), operand8!(rax)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, ast.node(*rhs), target);
                    target.push(asm!(mov, operand8!(var, addr_lhs), operand8!(rax)));
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
                        operand8!(rax),
                        operand8!(var, block.var_addr(id).unwrap())
                    ));
                    target.push(asm!(mov, operand8!(var, addr_lhs), operand8!(rax)));
                }
                Expression::NumberLiteral(num) => {
                    target.push(asm!(mov, operand8!(var, addr_lhs), operand8!(int, *num)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, &ast.node(*rhs), target);
                    target.push(asm!(mov, operand8!(var, addr_lhs), operand8!(rax)));
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
                target.push(asm!(add, operand8!(rsp), operand8!(int, block.stack_depth)));
            }
            target.push(asm!(func_ret));
        }
        Expression::ReturnVoid => {
            if block.has_vars {
                target.push(asm!(add, operand8!(rsp), operand8!(int, block.stack_depth)));
            }
            target.push(asm!(func_ret));
        }
        Expression::ReturnVal(val) => {
            match ast.expr(*val) {
                Expression::Identifier(id) => {
                    target.push(asm!(
                        mov,
                        operand8!(rax),
                        operand8!(var, block.var_addr(id).unwrap())
                    ));
                }
                Expression::NumberLiteral(num) => {
                    target.push(asm!(mov, operand8!(rax), operand8!(int, *num)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, &ast.node(*val), target);
                }
                _ => panic!("{:?} is not a valid expression", ast.expr(*val)),
            }
            if block.has_vars {
                target.push(asm!(add, operand8!(rsp), operand8!(int, block.stack_depth)));
            }
            target.push(asm!(func_ret));
        }
        _ => {}
    }
}

static ARG_REG_NAMES: [Operand; 6] = [
    operand8!(rdi),
    operand8!(rsi),
    operand8!(rdx),
    operand8!(rcx),
    operand8!(r8),
    operand8!(r9),
];
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
        match &node.expr {
            Expression::FuncCall(name, _) => {
                builtin_fns.add_if_needed(name, &mut program);
            }
            Expression::FuncDef(name, block) => {
                let mut func: Vec<ASMStatement> = vec![asm!(func_def, name)];
                if block.has_vars {
                    func.push(asm!(sub, operand8!(rsp), operand8!(int, block.stack_depth)));
                }
                for (i, (_, var_info)) in block.vars.iter().enumerate() {
                    if !var_info.is_arg {
                        // arguments always come before other variables
                        break;
                    }
                    func.push(asm!(
                    mov,
                    operand8!(var, var_info.addr).clone(),
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
            Expression::RawASM(code) => {
                if node.is_top_level {
                    program.data_sect.push(asm!(code));
                }
            }
            Expression::Label(name) => {
                program.data_sect.push(asm!(label, name));
            }
            _ => (),
        }
    }

    if !err_collector.errors.is_empty() {
        err_collector.print_errs();
        println!("could not compile due to errors listed above");
        panic!();
    }

    println!("{}", program.description());

    //program.gen_code()
    String::new()
}
