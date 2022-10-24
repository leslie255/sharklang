use super::shir::{SHIRConst, SHIRProgram, SHIRTopLevel, SHIR};
use mir::ir::{
    DataType, Instruction as MIRInstr, Operand, OperandContent, OperationType as MIROpcode,
    Program as MIRProgram, TopLevelElement as MIRTopLevel,
};
use std::rc::Rc;

struct Context {
    fn_ret_type: DataType,
    parent_loop_start_label: Option<Rc<String>>,
    parent_loop_end_label: Option<Rc<String>>,
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
                let mut context = Context {
                    fn_ret_type: *ret_type,
                    parent_loop_start_label: None,
                    parent_loop_end_label: None,
                };
                for shir in body {
                    let instr = compile_instr(shir, &mut context, &mut fn_body);
                    fn_body.push(instr);
                }
                mir_program
                    .content
                    .push(MIRTopLevel::FnDef(Rc::clone(name), fn_body));
            }
            SHIRTopLevel::StaticVar { name: _, val: _ } => todo!(),
            SHIRTopLevel::ExternFn { name, .. } => {
                mir_program
                    .content
                    .push(MIRTopLevel::Extern(Rc::clone(name)));
            }
        }
    }
    mir_program
}

fn compile_instr(shir: &SHIR, context: &mut Context, target: &mut Vec<MIRInstr>) -> MIRInstr {
    // TODO: when an operand is a function
    match shir {
        SHIR::Var(_, _)
        | SHIR::Const(_)
        | SHIR::Arg(_, _)
        | SHIR::Deref(_, _)
        | SHIR::TakeAddr(_) => panic!("{shir:?}"),
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
        SHIR::Break => MIRInstr {
            operation: MIROpcode::Jmp,
            operand0: Operand {
                dtype: DataType::Irrelavent,
                content: OperandContent::Label(Rc::clone(
                    context.parent_loop_end_label.as_ref().unwrap(),
                )),
            },
            operand1: Operand::default(),
        },
        SHIR::Continue => MIRInstr {
            operation: MIROpcode::Jmp,
            operand0: Operand {
                dtype: DataType::Irrelavent,
                content: OperandContent::Label(Rc::clone(
                    context.parent_loop_start_label.as_ref().unwrap(),
                )),
            },
            operand1: Operand::default(),
        },
        SHIR::Loop(loop_body, loop_id) => compile_loop(*loop_id, loop_body, context, target),
        SHIR::If {
            if_blocks,
            else_block,
        } => {
            if if_blocks.is_empty() {
                // if and else blocks may not be all empty as ensured in shir.rs
                let else_block = else_block.as_ref().unwrap();
                let mut last: MIRInstr =
                    compile_instr(else_block.first().unwrap(), context, target);
                for shir in else_block.iter().skip(1) {
                    target.push(last);
                    last = compile_instr(shir, context, target);
                }
                return last;
            }
            todo!("Generalized if statements")
        }
        SHIR::IfThenBreak(condition) => {
            let condition_mir = compile_oper(&condition, DataType::UnsignedSize, target);
            target.push(MIRInstr {
                operation: MIROpcode::Cmp,
                operand0: condition_mir,
                operand1: Operand {
                    dtype: DataType::UnsignedSize,
                    content: OperandContent::Data(0),
                },
            });
            MIRInstr {
                operation: MIROpcode::Jn,
                operand0: Operand {
                    dtype: DataType::Irrelavent,
                    content: OperandContent::Label(Rc::clone(
                        context.parent_loop_end_label.as_ref().unwrap(),
                    )),
                },
                operand1: Operand::default(),
            }
        }
        SHIR::IfThenContinue(condition) => {
            let condition_mir = compile_oper(&condition, DataType::UnsignedSize, target);
            target.push(MIRInstr {
                operation: MIROpcode::Cmp,
                operand0: condition_mir,
                operand1: Operand {
                    dtype: DataType::UnsignedSize,
                    content: OperandContent::Data(0),
                },
            });
            MIRInstr {
                operation: MIROpcode::Jn,
                operand0: Operand {
                    dtype: DataType::Irrelavent,
                    content: OperandContent::Label(Rc::clone(
                        context.parent_loop_start_label.as_ref().unwrap(),
                    )),
                },
                operand1: Operand::default(),
            }
        }
        SHIR::RawASM(code) => MIRInstr {
            operation: MIROpcode::RawASM,
            operand0: Operand {
                dtype: DataType::Irrelavent,
                content: OperandContent::RawASM(Rc::clone(code)),
            },
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
            SHIRConst::Char(ch) => Operand {
                dtype: expected_type,
                content: OperandContent::Data(*ch as u64),
            },
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
        SHIR::Deref(shir, dtype) => {
            let oper = compile_oper(&shir, DataType::Pointer, target);
            target.push(MIRInstr {
                operation: MIROpcode::Deref,
                operand0: oper,
                operand1: Operand {
                    dtype: *dtype,
                    content: OperandContent::Irrelavent,
                },
            });
            Operand {
                dtype: *dtype,
                content: OperandContent::Result,
            }
        }
        SHIR::TakeAddr(shir) => {
            let oper = compile_oper(&shir, DataType::Irrelavent, target);
            target.push(MIRInstr {
                operation: MIROpcode::TakeAddr,
                operand0: oper,
                operand1: Operand::default(),
            });
            Operand {
                dtype: DataType::Pointer,
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
    for (i, (arg_shir, arg_t)) in args.iter().enumerate().rev() {
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

#[must_use]
fn compile_loop(
    id: usize,
    body: &Vec<SHIR>,
    context: &mut Context,
    target: &mut Vec<MIRInstr>,
) -> MIRInstr {
    let loop_start_label = Rc::new(format!("___loop.start.{id}"));
    let loop_end_label = Rc::new(format!("___loop.end.{id}"));
    context.parent_loop_start_label = Some(Rc::clone(&loop_start_label));
    context.parent_loop_end_label = Some(Rc::clone(&loop_end_label));
    target.push(MIRInstr {
        operation: MIROpcode::Label,
        operand0: Operand {
            dtype: DataType::Irrelavent,
            content: OperandContent::Label(Rc::clone(&loop_start_label)),
        },
        operand1: Operand::default(),
    });
    for shir in body {
        let instr = compile_instr(shir, context, target);
        target.push(instr);
    }
    target.push(MIRInstr {
        operation: MIROpcode::Jmp,
        operand0: Operand {
            dtype: DataType::Irrelavent,
            content: OperandContent::Label(Rc::clone(&loop_start_label)),
        },
        operand1: Operand::default(),
    });
    MIRInstr {
        operation: MIROpcode::Label,
        operand0: Operand {
            dtype: DataType::Irrelavent,
            content: OperandContent::Label(Rc::clone(&loop_end_label)),
        },
        operand1: Operand::default(),
    }
}
