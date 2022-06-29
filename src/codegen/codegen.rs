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
        let mut func_call = ir!(func_call, func_name);
        let arg_types = ast.fn_arg_types(func_name); // TODO: promote size smaller than 32 bit to 32 bit in function call args
        for (i, arg) in args.iter().enumerate() {
            match ast.expr(*arg) {
                Expression::Identifier(id) => {
                    func_call.arg(operand!(
                        var,
                        arg_types[i].size(),
                        block.var_addr(id).unwrap()
                    ));
                }
                Expression::NumberLiteral(num) => {
                    func_call.arg(operand!(int, arg_types[i].size(), *num));
                }
                Expression::StringLiteral(str) => {
                    func_call.arg(operand!(
                        label,
                        arg_types[i].size(),
                        format!("strliteral_{}", program.strliterals_ids.get(str).unwrap())
                    ));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, ast, ast.node(*arg), target);
                    let reg = Operand {
                        len: arg_types[i].size() as usize,
                        addr_mode: AddrMode::Direct,
                        content: OperandContent::Reg(NESTED_FUNC_CALL_BUFFER_REGS[i]),
                    };
                    target.push(ir!(mov, reg.clone(), operand!(rax, arg_types[i].size())));
                    func_call.arg(reg);
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
        Expression::RawASM(text) => target.push(ir!(format!("\t{}", text))),
        Expression::Label(name) => target.push(ir!(label, name)),
        Expression::VarInit(var_name, var_type, rhs) => {
            let addr_lhs = block.var_addr(var_name).unwrap();
            let size = var_type.size();
            match ast.expr(*rhs) {
                Expression::Identifier(id) => {
                    let addr_rhs = block.var_addr(id).unwrap();
                    target.push(ir!(mov, operand!(rax, size), operand!(var, size, addr_rhs)));
                    target.push(ir!(mov, operand!(var, size, addr_lhs), operand!(rax, size)));
                }
                Expression::NumberLiteral(num) => {
                    target.push(ir!(mov, operand!(var, size, addr_lhs), operand!(int, size, *num)));
                }
                Expression::StringLiteral(str) => {
                    let strliteral_id =
                        format!("strliteral_{}", program.strliterals_ids.get(str).unwrap());
                    target.push(ir!(
                        mov,
                        operand!(rax, size),
                        operand!(label, size, strliteral_id)
                    ));
                    target.push(ir!(mov, operand!(var, size, addr_lhs), operand!(rax, size)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, ast.node(*rhs), target);
                    target.push(ir!(mov, operand!(var, size, addr_lhs), operand!(rax, size)));
                }
                _ => {
                    panic!();
                }
            };
        }
        Expression::VarAssign(var_name, rhs) => {
            let addr_lhs = block.var_addr(var_name).unwrap();
            let size = block.var_type(var_name).size();
            match ast.expr(*rhs) {
                Expression::Identifier(id) => {
                    target.push(ir!(
                        mov,
                        operand!(rax, size),
                        operand!(var, size, block.var_addr(id).unwrap())
                    ));
                    target.push(ir!(mov, operand!(var, size, addr_lhs), operand!(rax, size)));
                }
                Expression::NumberLiteral(num) => {
                    target.push(ir!(mov, operand!(var, size, addr_lhs), operand!(int, size, *num)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, &ast.node(*rhs), target);
                    target.push(ir!(mov, operand!(var, size, addr_lhs), operand!(rax, size)));
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
            match ast.expr(*val) {
                Expression::Identifier(id) => {
                    target.push(ir!(
                        mov,
                        operand!(rax, size),
                        operand!(var, size, block.var_addr(id).unwrap())
                    ));
                }
                Expression::NumberLiteral(num) => {
                    target.push(ir!(mov, operand!(rax, size), operand!(int, size, *num)));
                }
                Expression::FuncCall(_, _) => {
                    codegen_for_fn_call(block, program, &ast, &ast.node(*val), target);
                }
                _ => panic!("{:?} is not a valid expression", ast.expr(*val)),
            }
            if block.has_vars {
                target.push(ir!(
                    add,
                    operand!(rsp, 8),
                    operand!(int, 8, block.stack_depth)
                ));
            }
            target.push(ir!(func_ret));
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
            Expression::FuncDef(name, block) => {
                let mut func: Vec<ASMStatement> = vec![ir!(func_def, name)];
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
                    gen_code_inside_block(&block, &ast, ast.node(*i), &mut program, &mut func);
                }
                program.funcs.push(func);
            }
            Expression::RawASM(code) => {
                if node.is_top_level {
                    program.data_sect.push(ir!(code));
                }
            }
            Expression::Label(name) => {
                program.data_sect.push(ir!(label, name));
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

    let generated = program.gen_code();
    println!("\n----------\n{}", generated);
    generated
}
