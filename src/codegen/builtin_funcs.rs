use super::ir::*;

use std::collections::HashMap;
use std::collections::HashSet;

struct BuiltinFunc {
    externs: Vec<String>,
    data_sect: Vec<ASMStatement>,
    text_sect: Vec<ASMStatement>,
}

pub struct BuiltinFuncChecker {
    existing_funcs: HashSet<String>,
    existing_externs: HashSet<String>,
    existing_data: HashSet<ASMStatement>,

    funcs: HashMap<String, BuiltinFunc>,
}
impl BuiltinFuncChecker {
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
            },
        );
        checker.funcs.insert(
            String::from("print_int"),
            BuiltinFunc {
                externs: vec![String::from("printf")],
                data_sect: vec![asm!(data_str, "printint_fmt", "%d\n")],
                text_sect: vec![
                    asm!(func_def, "print_int"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(func_call, "printf")
                        .arg(addr!("printint_fmt"))
                        .arg(rax!())
                        .asm
                        .clone(),
                    asm!(func_ret),
                ],
            },
        );
        checker.funcs.insert(
            String::from("addint"),
            BuiltinFunc {
                externs: Vec::new(),
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "addint"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(add, rax!(), rsi!()),
                    asm!(func_ret),
                ],
            },
        );
        checker.funcs.insert(
            String::from("subint"),
            BuiltinFunc {
                externs: Vec::new(),
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "subint"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(sub, rax!(), rsi!()),
                    asm!(func_ret),
                ],
            },
        );
        checker.funcs.insert(
            String::from("mulint"),
            BuiltinFunc {
                externs: Vec::new(),
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "mulint"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(mul, rsi!()),
                    asm!(func_ret),
                ],
            },
        );
        checker.funcs.insert(
            String::from("divint"),
            BuiltinFunc {
                externs: Vec::new(),
                data_sect: Vec::new(),
                text_sect: vec![
                    asm!(func_def, "divint"),
                    asm!(mov, rax!(), rdi!()),
                    asm!(div, rsi!()),
                    asm!(func_ret),
                ],
            },
        );
        checker
    }
}
