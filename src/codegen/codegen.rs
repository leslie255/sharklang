use super::ast::*;
use super::builtin_funcs::*;
use super::error::*;
use super::ir::*;
use super::preprocess::*;
use super::tokens::*;
use super::typecheck::*;

#[allow(unused)]
static NESTED_FUNC_CALL_BUFFER_REGS: [Register; 6] = [
    Register::r10,
    Register::r11,
    Register::r12,
    Register::r13,
    Register::r14,
    Register::r15,
];

#[allow(unused_variables)]
fn codegen_for_simple_expr(
    block: &CodeBlock,
    program: &Program,
    ast: &AST,
    node: &ASTNode,
    target: &mut Vec<ASMStatement>,
    expected_size: u64,
) -> Operand {
    match &node.expr {
        Expression::Identifier(id) => {
            let var_info = block.var_info(id, ast).unwrap();
            operand!(var, expected_size, var_info.addr)
        }
        Expression::NumberLiteral(num) => {
            operand!(int, expected_size, *num)
        }
        Expression::StringLiteral(str) => {
            operand!(
                label,
                expected_size,
                format!("_strliteral_{}", program.strliterals_ids.get(str).unwrap())
            )
        }
        Expression::CharLiteral(byte) => {
            operand!(int, 1, *byte as u64)
        }
        Expression::FuncCall(_, _) => {
            codegen_for_fn_call(block, program, ast, node, target);
            operand!(rax, 8)
        }
        _ => panic!(),
    }
}

fn codegen_for_fn_call(
    block: &CodeBlock,
    program: &Program,
    ast: &AST,
    node: &ASTNode,
    target: &mut Vec<ASMStatement>,
) {
    if let Expression::FuncCall(func_name, args) = &node.expr {
        let mut func_call = ir!(func_call, func_name);
        // TODO: promote size smaller than 32 bit to 32 bit in function call args
        let arg_types = ast.fn_arg_types(func_name);
        for (i, arg) in args.iter().enumerate() {
            let size = if arg_types.is_some() {
                arg_types.unwrap()[i].size()
            } else {
                8
            };
            match ast.expr_no_typecast(*arg) {
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, ast, ast.node_no_typecast(*arg), target);
                    let reg = Operand {
                        len: size as usize,
                        addr_mode: AddrMode::Direct,
                        content: OperandContent::Reg(NESTED_FUNC_CALL_BUFFER_REGS[i]),
                    };
                    target.push(ir!(mov, reg.clone(), operand!(rax, size)));
                    func_call.arg(reg);
                }
                _ => {
                    func_call.arg(codegen_for_simple_expr(
                        block,
                        program,
                        ast,
                        ast.node_no_typecast(*arg),
                        target,
                        size,
                    ));
                }
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
        Expression::RawASM(text) => target.push(ir!(format!("\t{}", text))),
        Expression::Label(name) => target.push(ir!(label, name)),
        Expression::VarInit(var_name, var_type, rhs) => {
            let addr_lhs = block.var_addr(var_name, ast).unwrap();
            let size = var_type.size();
            let rhs_operand = codegen_for_simple_expr(
                block,
                program,
                ast,
                ast.node_no_typecast(*rhs),
                target,
                size,
            );
            target.push(ir!(mov, operand!(var, size, addr_lhs), rhs_operand));
        }
        Expression::VarAssign(var_name, rhs) => {
            let addr_lhs = block.var_addr(var_name, ast).unwrap();
            let size = block.var_type(var_name, ast).size();
            let rhs_operand = codegen_for_simple_expr(
                block,
                program,
                ast,
                ast.node_no_typecast(*rhs),
                target,
                size,
            );
            target.push(ir!(mov, operand!(var, size, addr_lhs), rhs_operand));
        }
        Expression::FuncCall(_, _) => {
            codegen_for_fn_call(block, program, &ast, node, target);
        }
        Expression::UnsafeReturn => {
            if block.has_vars {
                target.push(ir!(
                    add,
                    operand!(rsp, 8),
                    operand!(int, 8, block.stack_depth)
                ));
            }
            target.push(ir!(func_ret));
        }
        Expression::ReturnVoid => {
            if block.has_vars {
                target.push(ir!(
                    add,
                    operand!(rsp, 8),
                    operand!(int, 8, block.stack_depth)
                ));
            }
            target.push(ir!(func_ret));
        }
        Expression::ReturnVal(val) => {
            let size = block.return_type.size();
            let return_val = codegen_for_simple_expr(
                block,
                program,
                ast,
                ast.node_no_typecast(*val),
                target,
                size,
            );
            target.push(ir!(mov, operand!(rax, size), return_val));
            if block.has_vars {
                target.push(ir!(
                    add,
                    operand!(rsp, 8),
                    operand!(int, 8, block.stack_depth)
                ));
            }
            target.push(ir!(func_ret));
        }
        Expression::Loop(block_i) => {
            let label = format!("loop_{}", block_i);
            target.push(ir!(label, label.clone()));
            let loop_block = if let Expression::Block(b) = ast.expr_no_typecast(*block_i) {
                b
            } else {
                panic!();
            };
            let mut inside_loop: Vec<ASMStatement> = Vec::new();
            for i in &loop_block.body {
                gen_code_inside_block(
                    loop_block,
                    ast,
                    ast.node_no_typecast(*i),
                    program,
                    &mut inside_loop,
                );
            }
            target.append(&mut inside_loop);
            target.push(ir!(jmp, label));
        }
        _ => {}
    }
}

static ARG_REGS: [Register; 6] = [
    Register::rdi,
    Register::rsi,
    Register::rdx,
    Register::rcx,
    Register::r8,
    Register::r9,
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
                .push(ir!(data_str, format!("strliteral_{}", i), str));
        }
    }

    for node in &ast.nodes {
        match &node.expr {
            Expression::FuncCall(name, _) => {
                builtin_fns.add_if_needed(name, &mut program);
            }
            Expression::FuncDef(name, _) => {
                let mut func: Vec<ASMStatement> = vec![ir!(func_def, name)];
                let block = ast.fn_block(name).unwrap();
                if block.has_vars {
                    func.push(ir!(
                        sub,
                        operand!(rsp, 8),
                        operand!(int, 8, block.stack_depth)
                    ));
                }
                for (i, (_, var_info)) in block.vars.iter().enumerate() {
                    if !var_info.is_arg {
                        // arguments always come before other variables
                        break;
                    }
                    func.push(ir!(
                    mov,
                    operand!(var, var_info.data_type.size(), var_info.addr).clone(),
                    operand!(reg, ARG_REGS.get(i)
                        .expect("passing more than 6 arguments into a function haven't been implemented yet")
                        .clone(), var_info.data_type.size())
                ));
                }
                for i in &block.body {
                    gen_code_inside_block(
                        &block,
                        &ast,
                        ast.node_no_typecast(*i),
                        &mut program,
                        &mut func,
                    );
                }
                program.funcs.push(func);
            }
            Expression::RawASM(code) => {
                if node.is_top_level {
                    program.data_sect.push(ir!(code));
                }
            }
            Expression::Label(name) => {
                if node.is_top_level {
                    program.data_sect.push(ir!(label, name));
                }
            }
            _ => (),
        }
    }

    err_collector.print_errs();

    let generated = program.gen_code();
    generated
}
