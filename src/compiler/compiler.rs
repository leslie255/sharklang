use std::rc::Rc;

use super::shir::*;
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
                is_local: _,
                name,
                body,
                ret_type,
            } => {
                let mut fn_body = Vec::<MIRInstr>::new();
                let context = Context {
                    fn_ret_type: *ret_type,
                };
                for shir in body {
                    let instr = compile_instr(shir, &context, &mut fn_body);
                    fn_body.push(instr);
                }
                mir_program
                    .content
                    .push(MIRTopLevel::FnDef(Rc::clone(name), fn_body));
            }
            SHIRTopLevel::StaticVar { name: _, val: _ } => todo!(),
            SHIRTopLevel::ExternFn { name, ret_type: _ } => {
                mir_program
                    .content
                    .push(MIRTopLevel::Extern(Rc::clone(name)));
            }
        }
    }
    mir_program
}

fn compile_instr(shir: &SHIR, context: &Context, target: &mut Vec<MIRInstr>) -> MIRInstr {
    // TODO: when an operand is a function
    match shir {
        SHIR::Var(_, _) | SHIR::Const(_) | SHIR::Arg(_, _) => panic!(),
        SHIR::VarAssign { id, dtype, rhs } => MIRInstr {
            operation: MIROpcode::SetVar,
            operand0: Operand {
                dtype: *dtype,
                content: OperandContent::Var(*id),
            },
            operand1: compile_oper(rhs, *dtype, target),
        },
        SHIR::VarDef { id, dtype } => MIRInstr {
            operation: MIROpcode::DefVar,
            operand0: Operand {
                dtype: *dtype,
                content: OperandContent::Var(*id),
            },
            operand1: Operand::default(),
        },
        SHIR::FnCall {
            name,
            args,
            ret_type: _,
        } => compile_fn_call(name, args, target),
        SHIR::ReturnVoid => MIRInstr {
            operation: MIROpcode::RetVoid,
            operand0: Operand::default(),
            operand1: Operand::default(),
        },
        SHIR::ReturnValue(val) => MIRInstr {
            operation: MIROpcode::RetVal,
            operand0: compile_oper(val, context.fn_ret_type, target),
            operand1: Operand::default(),
        },
    }
}

fn compile_oper(shir: &SHIR, expected_type: DataType, target: &mut Vec<MIRInstr>) -> Operand {
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
                content: OperandContent::Label(Rc::new(format!("strliteral_{str_id}"))),
            },
            SHIRConst::Char(_) => todo!(),
        },
        SHIR::Arg(id, dtype) => Operand {
            dtype: *dtype,
            content: OperandContent::Arg(*id),
        },
        SHIR::FnCall {
            name,
            args,
            ret_type,
        } => {
            let fn_call = compile_fn_call(name, args, target);
            target.push(fn_call);
            Operand {
                dtype: *ret_type,
                content: OperandContent::Result,
            }
        }
        _ => panic!(),
    }
}

#[must_use]
fn compile_fn_call(
    name: &Rc<String>,
    args: &Vec<(SHIR, DataType)>,
    target: &mut Vec<MIRInstr>,
) -> MIRInstr {
    for (i, (arg_shir, arg_t)) in args.iter().rev().enumerate() {
        let rhs_oper = compile_oper(arg_shir, *arg_t, target);
        target.push(MIRInstr {
            operation: MIROpcode::SetArg,
            operand0: Operand {
                dtype: *arg_t,
                content: OperandContent::Arg(i as u64),
            },
            operand1: rhs_oper,
        });
    }
    MIRInstr {
        operation: MIROpcode::CallFn,
        operand0: Operand {
            dtype: DataType::Irrelavent,
            content: OperandContent::Fn(Rc::clone(name)),
        },
        operand1: Operand::default(),
    }
}
