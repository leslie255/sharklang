use std::collections::HashMap;

use super::ast::*;
use super::builtin_funcs::BuiltinFuncChecker;
use super::checks::syntaxcheck::*;
use super::checks::typecheck::*;
use super::error::*;
use super::preprocess::*;
use super::tokens::*;
use super::typeinfer::*;
use mir::fileformat::*;
use mir::ir::*;

#[allow(unused)]
struct Context<'a> {
    ast: &'a AST,
    str_literals: &'a HashMap<String, u64>,
    parent_block: &'a CodeBlock,
    target: &'a mut Vec<Instruction>,
}

fn gen_str_literals(ast: &AST, target: &mut Program) -> HashMap<String, u64> {
    let mut str_literal_table = HashMap::<String, u64>::new();
    for (i, string) in ast.nodes.iter().enumerate().filter_map(|(i, n)| {
        if let Expression::StringLiteral(s) = &n.expr {
            Some((i, s))
        } else {
            None
        }
    }) {
        str_literal_table.insert(string.clone(), i as u64);
        target.content.push(TopLevelElement::DataStr(
            format!("strliteral_{}", i),
            string.clone(),
        ));
    }
    str_literal_table
}

fn gen_ir_inside_fn(context: &mut Context, node: &ASTNode) {
    match &node.expr {
        Expression::Identifier(_) => todo!(),
        Expression::NumberLiteral(_) => todo!(),
        Expression::StringLiteral(_) => todo!(),
        Expression::CharLiteral(_) => todo!(),
        Expression::FuncCall(_, _) => todo!(),
        Expression::VarInit(_, _, _) => todo!(),
        Expression::VarAssign(_, _) => todo!(),
        Expression::TypeCast(_, _) => todo!(),
        Expression::GetAddress(_) => todo!(),
        Expression::Dereference(_) => todo!(),
        Expression::Label(_) => todo!(),
        Expression::RawASM(asm_code) => context.target.push(Instruction {
            operation: OperationType::RawASM,
            operand0: Operand {
                dtype: mir::ir::DataType::Irrelavent,
                content: OperandContent::RawASM(asm_code.clone()),
            },
            operand1: Operand {
                dtype: mir::ir::DataType::Irrelavent,
                content: OperandContent::Irrelavent,
            },
        }),
        Expression::Block(_) => todo!(),
        Expression::FuncDef(_, _) => todo!(),
        Expression::Loop(_) => todo!(),
        Expression::If(_, _, _, _) => todo!(),
        Expression::ReturnVoid => context.target.push(Instruction {
            operation: OperationType::Ret,
            operand0: Operand {
                dtype: mir::ir::DataType::Unsigned64,
                content: OperandContent::Data(0),
            },
            operand1: Operand {
                dtype: mir::ir::DataType::Irrelavent,
                content: OperandContent::Irrelavent,
            },
        }),
        Expression::ReturnVal(node_i) => {
            let expr = context.ast.expr(*node_i);
            context.target.push(Instruction {
                operation: OperationType::Ret,
                operand0: Operand {
                    dtype: mir::ir::DataType::Unsigned64,
                    content: OperandContent::Data(0),
                },
                operand1: Operand {
                    dtype: mir::ir::DataType::Irrelavent,
                    content: OperandContent::Irrelavent,
                },
            })
        }
        Expression::UnsafeReturn => context.target.push(Instruction {
            operation: OperationType::Ret,
            operand0: Operand {
                dtype: mir::ir::DataType::Unsigned64,
                content: OperandContent::Data(0),
            },
            operand1: Operand {
                dtype: mir::ir::DataType::Irrelavent,
                content: OperandContent::Irrelavent,
            },
        }),
        Expression::Break => todo!(),
        Expression::Continue => todo!(),
        Expression::Unknown => todo!(),
    }
}

fn gen_ir_toplevel(ast: &AST, target: &mut Program, str_literals: &HashMap<String, u64>) {
    for (fn_name, fn_block) in ast.nodes.iter().filter_map(|n| {
        if let Expression::FuncDef(fn_name, block_i) = &n.expr {
            Some((fn_name, ast.expr(*block_i).get_block_unchecked()))
        } else {
            None
        }
    }) {
        let mut fn_body = Vec::<Instruction>::new();
        let mut context = Context {
            ast,
            str_literals,
            parent_block: fn_block,
            target: &mut fn_body,
        };
        for node in fn_block
            .body
            .iter()
            .map(|i| ast.node(*i))
            .collect::<Vec<&ASTNode>>()
        {
            gen_ir_inside_fn(&mut context, node);
        }
        let def_fn = TopLevelElement::FnDef(
            fn_name.clone(),
            fn_block
                .var_infos
                .iter()
                .map(|f| (f.0.clone(), f.1.data_type.mir_type().unwrap()))
                .collect(),
            fn_body,
        );
        target.content.push(def_fn);
    }
}

pub fn compile(source: String, src_file: String, file_format: FileFormat) -> String {
    let mut err_collector = ErrorCollector::new(src_file, &source);
    let tokens = preprocess(parse_tokens(&source));
    let mut ast = construct_ast(tokens, &mut err_collector);

    syntax_check(&mut err_collector, &ast);
    err_collector.print_errs();

    infer_type(&mut ast, &mut err_collector);
    err_collector.print_errs();

    let builtin_fns = BuiltinFuncChecker::new();

    print_ast!(ast.nodes);
    type_check(&ast, &builtin_fns, &mut err_collector);
    err_collector.print_errs();

    let mut program = Program::default();

    // first generate all string literals
    let str_literals = gen_str_literals(&ast, &mut program);
    gen_ir_toplevel(&ast, &mut program, &str_literals);

    err_collector.print_errs();

    println!("{:#?}", program.content);

    let generated = mir::generation::x86_64::generate_asm(program, file_format);
    generated
}
