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
                    func_call.arg(Operand::LocalVar(*block.var_addrs.get(id).unwrap()));
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
        Expression::VarInit(var_name, rhs) => {
            let addr_lhs = *block.var_addrs.get(var_name).unwrap();
            match ast.expr(*rhs) {
                Expression::Identifier(id) => {
                    let addr_rhs = *block.var_addrs.get(id).unwrap();
                    target.push(asm!(mov, rax!(), Operand::LocalVar(addr_rhs)));
                    target.push(asm!(mov, Operand::LocalVar(addr_lhs), rax!()));
                }
                Expression::NumberLiteral(num) => {
                    target.push(asm!(mov, Operand::LocalVar(addr_lhs), Operand::Int(*num)));
                }
                Expression::StringLiteral(_) => {
                    println!("using string literal as the rhs of variable declaration has not been implemented yet");
                    todo!();
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
            let addr_lhs = *block.var_addrs.get(var_name).unwrap();
            match ast.expr(*rhs) {
                Expression::Identifier(id) => {
                    target.push(asm!(
                        mov,
                        rax!(),
                        Operand::LocalVar(*block.var_addrs.get(id).unwrap())
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
        Expression::ReturnVoid => {
            if block.total_var_bytes != 0 {
                target.push(asm!(add, rsp!(), Operand::Int(block.total_var_bytes)));
            }
            target.push(asm!(func_ret));
        }
        Expression::ReturnVal(val) => {
            match ast.expr(*val) {
                Expression::Identifier(id) => {
                    target.push(asm!(
                        mov,
                        rax!(),
                        Operand::LocalVar(*block.var_addrs.get(id).unwrap())
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
            if block.total_var_bytes != 0 {
                target.push(asm!(add, rsp!(), Operand::Int(block.total_var_bytes)));
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
            if block.total_var_bytes != 0 {
                func.push(asm!(sub, rsp!(), Operand::Int(block.total_var_bytes)));
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

    program.gen_code()
}
