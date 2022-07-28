use super::ast::*;
use super::builtin_funcs::*;
use super::checks::syntaxcheck::*;
use super::checks::typecheck::*;
use super::error::*;
use super::ir::*;
use super::preprocess::*;
use super::tokens::*;
use super::typeinfer::*;

static NESTED_FUNC_CALL_BUFFER_REGS: [Register; 6] = [
    Register::r10,
    Register::r11,
    Register::r12,
    Register::r13,
    Register::r14,
    Register::r15,
];

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
            println!("{}:{}\t{}", file!(), line!(), str);
            operand!(label, program.strliterals.get_label(str))
        }
        Expression::CharLiteral(byte) => {
            operand!(int, 1, *byte as u64)
        }
        Expression::FuncCall(_, _) => {
            codegen_for_fn_call(block, program, ast, node, target);
            operand!(rax, 8)
        }
        Expression::GetAddress(expr_i) => match &ast.expr(*expr_i) {
            Expression::Identifier(id) => {
                let var_info = block.var_info(id, ast).unwrap();
                Operand {
                    len: expected_size as usize,
                    addr_mode: AddrMode::Direct,
                    content: OperandContent::GetVarAddr(var_info.addr),
                }
            }
            _ => panic!(),
        },
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
        let arg_types = ast.fn_args(func_name);
        for (i, arg) in args.iter().enumerate() {
            let size = if arg_types.is_some() {
                arg_types.unwrap()[i].1.size()
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
    builtin_fns: &mut BuiltinFuncChecker,
    file_format: FileFormat,
) {
    match &node.expr {
        Expression::Identifier(_) => {}
        Expression::NumberLiteral(_) => {}
        Expression::StringLiteral(str) => {
            // implicit `println` call
            builtin_fns.add_if_needed(&format!("println"), program);
            target.push(
                ir!(func_call, "println")
                    .arg(operand!(label, program.strliterals.get_label(str)))
                    .asm
                    .clone(),
            );
        }
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
            let size = block.fn_return_type(ast).unwrap_or(DataType::Void).size();
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
        Expression::Break => {
            // break and continue has been made sure to be inside a loop in syntax checker
            target.push(ir!(
                jmp,
                format!("break_{}", block.parent_loop_id(ast).unwrap())
            ));
        }
        Expression::Continue => {
            target.push(ir!(
                jmp,
                format!("loop_{}", block.parent_loop_id(ast).unwrap())
            ));
        }
        Expression::Loop(block_i) => {
            let label = format!("loop_{}", block_i);
            let break_label = format!("break_{}", block_i);
            target.push(ir!(label, label.clone()));
            let loop_block = ast.expr_no_typecast(*block_i).get_block_unchecked();
            let mut inside_loop: Vec<ASMStatement> = Vec::new();
            for i in &loop_block.body {
                gen_code_inside_block(
                    loop_block,
                    ast,
                    ast.node_no_typecast(*i),
                    program,
                    &mut inside_loop,
                    builtin_fns,
                    file_format,
                );
            }
            target.append(&mut inside_loop);
            target.push(ir!(jmp, label));
            target.push(ir!(label, break_label.clone()));
        }
        #[allow(unused_variables)]
        Expression::If(condition_i, if_block_i, elif_blocks, else_block_i) => {
            let end_label = format!("if_end_{}", if_block_i);
            let else_label = format!("else_{}", if_block_i);
            let condition_operand = codegen_for_simple_expr(
                block,
                program,
                ast,
                ast.node_no_typecast(*condition_i),
                target,
                8,
            );
            target.push(ir!(format!(
                "\tmov\trax, {}",
                condition_operand.text(file_format)
            )));
            target.push(ir!(format!("\tcmp\trax, 0",)));
            target.push(ir!(format!(
                "\tje\t{}",
                if *else_block_i == usize::MAX {
                    end_label.clone()
                } else {
                    else_label.clone()
                }
            )));
            let if_block = ast.expr_no_typecast(*if_block_i).get_block_unchecked();
            let mut inside_if: Vec<ASMStatement> = Vec::new();
            for i in &if_block.body {
                gen_code_inside_block(
                    if_block,
                    ast,
                    ast.node_no_typecast(*i),
                    program,
                    &mut inside_if,
                    builtin_fns,
                    file_format,
                );
            }
            target.append(&mut inside_if);
            if *else_block_i != usize::MAX {
                target.push(ir!(format!("\tjmp\t{}", end_label.clone())));
            }
            if let Some(else_block) = ast.expr_no_typecast(*else_block_i).get_block() {
                let mut inside_else: Vec<ASMStatement> = Vec::new();
                target.push(ir!(label, else_label));
                for i in &else_block.body {
                    gen_code_inside_block(
                        if_block,
                        ast,
                        ast.node_no_typecast(*i),
                        program,
                        &mut inside_else,
                        builtin_fns,
                        file_format,
                    );
                }
                target.append(&mut inside_else);
            }
            target.push(ir!(label, end_label));
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
pub fn compile(source: String, src_file: String, file_format: FileFormat) -> String {
    let mut err_collector = ErrorCollector::new(src_file, &source);
    let tokens = preprocess(parse_tokens(&source));
    let mut ast = construct_ast(tokens, &mut err_collector);

    syntax_check(&mut err_collector, &ast);
    err_collector.print_errs();

    infer_type(&mut ast, &mut err_collector);
    err_collector.print_errs();

    let mut builtin_fns = BuiltinFuncChecker::new();

    let mut ast_changes: Vec<(usize, CodeBlock)> = Vec::new();
    for (i, block) in ast.nodes.iter().enumerate().filter_map(|(i, n)| {
        if let Some(b) = n.expr.get_block() {
            Some((i, b))
        } else {
            None
        }
    }) {
        let mut new_block = block.clone();
        new_block.gen_var_addrs(&ast);
        ast_changes.push((i, new_block));
    }
    for (i, new_block) in ast_changes {
        *ast.node_mut(i).expr.get_block_mut().unwrap() = new_block;
    }

    //print_ast!(ast.nodes);
    type_check(&ast, &builtin_fns, &mut err_collector);
    err_collector.print_errs();

    let mut program = Program::new(file_format);

    // generate string literals
    for (i, node) in ast.nodes.iter().enumerate() {
        if let Expression::StringLiteral(str) = &node.expr {
            program.strliterals.add(str.clone(), i);
            program
                .data_sect
                .push(ir!(data_str, program.strliterals.get_label(str), str));
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
                        &mut builtin_fns,
                        file_format,
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

    //println!("{}", program.description());

    let generated = program.gen_code();
    generated
}
