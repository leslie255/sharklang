use std::collections::HashMap;

use super::ast::*;
use super::builtin_funcs::BuiltinFuncChecker;
use super::checks::syntaxcheck::*;
use super::checks::typecheck::type_check;
use super::error::*;
use super::preprocess::*;
use super::tokens::*;
use super::typeinfer::*;
use mir::fileformat::*;
use mir::ir::*;

struct Context<'a> {
    ast: &'a AST,
    str_literals: &'a HashMap<String, u64>,
    parent_block: &'a CodeBlock,
    target: &'a mut Vec<Instruction>,
}
impl<'a> Context<'a> {
    fn get_str_literal_id(&self, string: &String) -> String {
        format!("strliteral_{}", self.str_literals.get(string).unwrap())
    }
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

fn gen_ir_for_simple_expr(context: &mut Context, node: &ASTNode) -> Operand {
    match &node.expr {
        Expression::Identifier(id) => Operand {
            dtype: context
                .parent_block
                .var_info(id, context.ast)
                .unwrap()
                .data_type
                .mir_type()
                .unwrap(),
            content: OperandContent::Var(id.clone()),
        },
        Expression::NumberLiteral(num) => Operand {
            dtype: context
                .parent_block
                .fn_return_type(context.ast)
                .unwrap()
                .mir_type()
                .unwrap(),
            content: OperandContent::Data(*num),
        },
        Expression::StringLiteral(str) => Operand {
            dtype: mir::ir::DataType::Irrelavent,
            content: OperandContent::Label(context.get_str_literal_id(str)),
        },
        Expression::CharLiteral(num) => Operand {
            dtype: mir::ir::DataType::Unsigned8,
            content: OperandContent::Data(*num as u64),
        },
        Expression::TypeCast(_, _) => todo!(),
        Expression::FuncCall(_, _) => todo!(),
        Expression::VarInit(_, _, _) => panic!(),
        Expression::VarAssign(_, _) => panic!(),
        Expression::GetAddress(_) => panic!(),
        Expression::Dereference(_) => panic!(),
        Expression::Label(_) => panic!(),
        Expression::RawASM(_) => panic!(),
        Expression::Block(_) => panic!(),
        Expression::FuncDef(_, _) => panic!(),
        Expression::Loop(_) => panic!(),
        Expression::If(_, _, _, _) => panic!(),
        Expression::ReturnVoid => panic!(),
        Expression::ReturnVal(_) => panic!(),
        Expression::UnsafeReturn => panic!(),
        Expression::Break => panic!(),
        Expression::Continue => panic!(),
        Expression::Unknown => panic!(),
    }
}

fn gen_ir_inside_fn(context: &mut Context, node: &ASTNode) {
    match &node.expr {
        Expression::Identifier(_)
        | Expression::NumberLiteral(_)
        | Expression::CharLiteral(_)
        | Expression::TypeCast(_, _)
        | Expression::GetAddress(_)
        | Expression::Dereference(_)
        | Expression::FuncDef(_, _)
        | Expression::Block(_) => panic!(),
        Expression::StringLiteral(_) => todo!(),
        Expression::FuncCall(_, _) => todo!(),
        Expression::VarInit(var_name, _, rhs_i) | Expression::VarAssign(var_name, rhs_i) => {
            let rhs_oper = gen_ir_for_simple_expr(context, context.ast.node(*rhs_i));
            context.target.push(Instruction {
                operation: OperationType::SetVar,
                operand0: Operand {
                    dtype: (&context
                        .parent_block
                        .var_info(var_name, context.ast)
                        .unwrap()
                        .data_type)
                        .mir_type()
                        .unwrap(),
                    content: OperandContent::Var(var_name.clone()),
                },
                operand1: rhs_oper,
            })
        }
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
            gen_ir_for_simple_expr(context, context.ast.node(*node_i));
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
