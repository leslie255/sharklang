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

fn gen_ir_for_fn_call(context: &mut Context, fn_name: &String, args: &Vec<usize>) {
    let arg_types = &context.ast.fn_block(fn_name).unwrap().args;
    for (arg_i, node) in args.iter().map(|i| context.ast.node(*i)).enumerate() {
        let arg_operand = gen_ir_for_simple_expr(context, node);
        context.target.push(Instruction {
            operation: OperationType::SetArg,
            operand0: Operand {
                dtype: arg_types.get(arg_i).unwrap().1.mir_type().unwrap(),
                content: OperandContent::Arg(arg_i as u64),
            },
            operand1: arg_operand,
        });
    }
    context.target.push(Instruction {
        operation: OperationType::CallFn,
        operand0: Operand {
            dtype: mir::ir::DataType::Irrelavent,
            content: OperandContent::Fn(fn_name.clone()),
        },
        operand1: Operand {
            dtype: mir::ir::DataType::Irrelavent,
            content: OperandContent::Irrelavent,
        },
    });
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
        Expression::FuncCall(fn_name, args) => {
            gen_ir_for_fn_call(context, fn_name, args);
            Operand {
                dtype: context
                    .ast
                    .fn_block(fn_name)
                    .unwrap()
                    .return_type
                    .mir_type()
                    .unwrap(),
                content: OperandContent::Result,
            }
        }
        Expression::TypeCast(_, _) => todo!(),
        Expression::VarInit(_, _, _)
        | Expression::VarAssign(_, _)
        | Expression::GetAddress(_)
        | Expression::Dereference(_)
        | Expression::Label(_)
        | Expression::RawASM(_)
        | Expression::Block(_)
        | Expression::FuncDef(_, _)
        | Expression::Loop(_)
        | Expression::If(_, _, _, _)
        | Expression::ReturnVoid
        | Expression::ReturnVal(_)
        | Expression::UnsafeReturn
        | Expression::Break
        | Expression::Continue
        | Expression::Unknown => panic!(),
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
        Expression::StringLiteral(string) => {
            context.target.push(Instruction {
                operation: OperationType::SetArg,
                operand0: Operand {
                    dtype: mir::ir::DataType::Unsigned64,
                    content: OperandContent::Arg(0),
                },
                operand1: Operand {
                    dtype: mir::ir::DataType::Irrelavent,
                    content: OperandContent::Label(context.get_str_literal_id(string)),
                },
            });
            context.target.push(Instruction {
                operation: OperationType::CallFn,
                operand0: Operand {
                    dtype: mir::ir::DataType::Irrelavent,
                    content: OperandContent::Fn("println".to_string()),
                },
                operand1: Operand {
                    dtype: mir::ir::DataType::Irrelavent,
                    content: OperandContent::Irrelavent,
                },
            });
        }
        Expression::FuncCall(fn_name, args) => {
            gen_ir_for_fn_call(context, fn_name, args);
        }
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
        Expression::Label(name) => context.target.push(Instruction {
            operation: OperationType::Label,
            operand0: Operand {
                dtype: DataType::Irrelavent,
                content: OperandContent::Label(name.clone()),
            },
            operand1: Operand {
                dtype: DataType::Irrelavent,
                content: OperandContent::Irrelavent,
            },
        }),
        Expression::RawASM(asm_code) => context.target.push(Instruction {
            operation: OperationType::RawASM,
            operand0: Operand {
                dtype: mir::ir::DataType::Irrelavent,
                content: OperandContent::RawASM(asm_code.clone()),
            },
            operand1: Operand::default(),
        }),
        Expression::Loop(_) => todo!(),
        Expression::If(_, _, _, _) => todo!(),
        Expression::ReturnVoid => context.target.push(Instruction {
            operation: OperationType::RetVoid,
            operand0: Operand::default(),
            operand1: Operand::default(),
        }),
        Expression::ReturnVal(node_i) => {
            let ret_operand = gen_ir_for_simple_expr(context, context.ast.node(*node_i));
            context.target.push(Instruction {
                operation: OperationType::RetVal,
                operand0: ret_operand,
                operand1: Operand::default(),
            })
        }
        Expression::UnsafeReturn => context.target.push(Instruction {
            operation: OperationType::RetVoid,
            operand0: Operand::default(),
            operand1: Operand::default(),
        }),
        Expression::Break => todo!(),
        Expression::Continue => todo!(),
        Expression::Unknown => panic!(),
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

        // generate argument variables
        for (i, (arg_name, arg_type)) in fn_block.args.iter().enumerate() {
            fn_body.push(Instruction {
                operation: OperationType::SetVar,
                operand0: Operand {
                    dtype: arg_type.mir_type().unwrap(),
                    content: OperandContent::Var(arg_name.clone()),
                },
                operand1: Operand {
                    dtype: arg_type.mir_type().unwrap(),
                    content: OperandContent::Arg(i as u64),
                },
            })
        }

        // generate function body
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
