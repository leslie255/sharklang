use super::ir::*;
use super::typecheck::*;

use std::collections::HashMap;
use std::collections::HashSet;

pub struct BuiltinFunc {
    pub externs: Vec<String>,
    pub data_sect: Vec<ASMStatement>,
    pub text_sect: Vec<ASMStatement>,
    pub return_type: DataType,
}

pub struct BuiltinFuncChecker {
    pub existing_funcs: HashSet<String>,
    pub existing_externs: HashSet<String>,
    pub existing_data: HashSet<ASMStatement>,

    pub funcs: HashMap<String, BuiltinFunc>,
}
impl BuiltinFuncChecker {
    pub fn is_builtin_func(&self, name: &String) -> bool {
        self.funcs.contains_key(name)
    }
    pub fn add_if_needed(&mut self, name: &String, program: &mut Program) {
        if !self.funcs.contains_key(name) {
            return;
        }
        if self.existing_funcs.contains(name) {
            return;
        }
        self.existing_funcs.insert(name.clone());
        let func = self.funcs.get(name).unwrap();
        for func_extern in &func.externs {
            if !self.existing_externs.contains(func_extern) {
                program.externs.push(asm!(extern, func_extern));
                self.existing_externs.insert(func_extern.clone());
            }
        }
        for item in &func.data_sect {
            if !self.existing_data.contains(item) {
                program.data_sect.push(item.clone());
                self.existing_data.insert(item.clone());
            }
        }
        program.funcs.push(func.text_sect.clone());
    }
    pub fn new() -> BuiltinFuncChecker {
        let mut checker = BuiltinFuncChecker {
            existing_funcs: HashSet::new(),
            existing_externs: HashSet::new(),
            existing_data: HashSet::new(),
            funcs: HashMap::new(),
        };

        checker.funcs.insert(
            String::from("print"),
            BuiltinFunc {
                externs: vec![String::from("printf")],
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "print"),
                    asm!(func_call, "printf").asm,
                    asm!(func_ret),
                ],
                return_type: DataType::Void,
            },
        );
        checker.funcs.insert(
            String::from("println"),
            BuiltinFunc {
                externs: vec![String::from("printf")],
                data_sect: vec![asm!(data_str, "println_fmt", "%s\n")],
                text_sect: vec![
                    asm!(func_def, "println"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(func_call, "printf")
                        .arg(addr!("println_fmt"))
                        .arg(rax!())
                        .asm
                        .clone(),
                    asm!(func_ret),
                ],
                return_type: DataType::Void,
            },
        );
        checker.funcs.insert(
            String::from("u64print"),
            BuiltinFunc {
                externs: vec![String::from("printf")],
                data_sect: vec![asm!(data_str, "u64_print_fmt", "%llu\n")],
                text_sect: vec![
                    asm!(func_def, "u64print"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(func_call, "printf")
                        .arg(addr!("u64_print_fmt"))
                        .arg(rax!())
                        .asm
                        .clone(),
                    asm!(func_ret),
                ],
                return_type: DataType::Void,
            },
        );
        checker.funcs.insert(
            String::from("uadd"),
            BuiltinFunc {
                externs: Vec::new(),
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "uadd"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(add, rax!(), rsi!()),
                    asm!(func_ret),
                ],
                return_type: DataType::UInt64,
            },
        );
        checker.funcs.insert(
            String::from("usub"),
            BuiltinFunc {
                externs: Vec::new(),
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "usub"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(sub, rax!(), rsi!()),
                    asm!(func_ret),
                ],
                return_type: DataType::UInt64,
            },
        );
        checker.funcs.insert(
            String::from("umul"),
            BuiltinFunc {
                externs: Vec::new(),
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "umul"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(mul, rsi!()),
                    asm!(func_ret),
                ],
                return_type: DataType::UInt64,
            },
        );
        checker.funcs.insert(
            String::from("udiv"),
            BuiltinFunc {
                externs: Vec::new(),
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "udiv"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(div, rsi!()),
                    asm!(func_ret),
                ],
                return_type: DataType::UInt64,
            },
        );
        checker
    }
}
