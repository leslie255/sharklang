use std::rc::Rc;

use super::{ast::NumValue, shir::*};
use mir::ir::{
    DataType, Instruction as MIRInstr, Operand, OperandContent, OperationType as MIROpcode,
    Program as MIRProgram, TopLevelElement as MIRTopLevel,
};

struct Context {
    fn_ret_type: DataType,
}

pub fn compile_shir_into_mir(shir_program: SHIRProgram) -> MIRProgram {
    let mut mir_program = MIRProgram::default();
    for (i, str) in shir_program.strliteral_pool.iter().enumerate() {
        mir_program.content.push(MIRTopLevel::DataStr(
            format!("strliteral_{i}"),
            str.to_string(),
        ));
    }
    for shir_top_level in &shir_program.body {
        match shir_top_level {
            SHIRTopLevel::Fn {
                is_local,
                name,
                body,
                ret_type,
            } => {
                let mut fn_body = Vec::<MIRInstr>::new();
                let context = Context {
                    fn_ret_type: *ret_type,
                };
                for shir in body {
                    fn_body.append(&mut compile_instr(shir, &context));
                }
                mir_program
                    .content
                    .push(MIRTopLevel::FnDef(Rc::clone(name), fn_body));
            }
            SHIRTopLevel::StaticVar { name, val } => todo!(),
        }
    }
    mir_program
}

fn compile_instr(shir: &SHIR, context: &Context) -> Vec<MIRInstr> {
    match shir {
        SHIR::Var(_, _) | SHIR::Const(_) => panic!(),
        SHIR::VarAssign { id, dtype, rhs } => vec![MIRInstr {
            operation: MIROpcode::SetVar,
            operand0: Operand {
                dtype: *dtype,
                content: OperandContent::Var(*id),
            },
            operand1: compile_oper(rhs, *dtype),
        }],
        SHIR::VarDef { id, dtype } => vec![MIRInstr {
            operation: MIROpcode::DefVar,
            operand0: Operand {
                dtype: *dtype,
                content: OperandContent::Var(*id),
            },
            operand1: Operand::default(),
        }],
        SHIR::FnCall {
            name,
            args,
            ret_type,
        } => {
            let mut result = Vec::<MIRInstr>::new();
            for (i, (arg_shir, arg_t)) in args.iter().enumerate() {
                result.push(MIRInstr {
                    operation: MIROpcode::SetArg,
                    operand0: Operand {
                        dtype: *arg_t,
                        content: OperandContent::Arg(i as u64),
                    },
                    operand1: compile_oper(arg_shir, *arg_t),
                });
            }
            result.push(MIRInstr {
                operation: MIROpcode::CallFn,
                operand0: Operand {
                    dtype: DataType::Irrelavent,
                    content: OperandContent::Fn(Rc::clone(name)),
                },
                operand1: Operand::default(),
            });
            result
        }
        SHIR::ReturnVoid => vec![MIRInstr {
            operation: MIROpcode::RetVoid,
            operand0: Operand::default(),
            operand1: Operand::default(),
        }],
        SHIR::ReturnValue(val) => vec![MIRInstr {
            operation: MIROpcode::RetVal,
            operand0: compile_oper(val, context.fn_ret_type),
            operand1: Operand::default(),
        }],
    }
}

fn compile_oper(shir: &SHIR, expected_type: DataType) -> Operand {
    match shir {
        SHIR::Var(id, dtype) => Operand {
            dtype: *dtype,
            content: OperandContent::Var(*id),
        },
        SHIR::Const(const_val) => match const_val {
            SHIRConst::Number(numval) => Operand {
                dtype: expected_type,
                content: OperandContent::Data(numval.into_u64_bytes()),
            },
            SHIRConst::String(str_id) => Operand {
                dtype: expected_type,
                content: OperandContent::SVar(Rc::new(format!("strliteral_{str_id}"))),
            },
            SHIRConst::Char(ch) => todo!(),
        },
        _ => panic!(),
    }
}
