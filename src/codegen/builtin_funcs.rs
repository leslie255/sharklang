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
    existing_funcs: HashSet<String>,
    existing_externs: HashSet<String>,
    existing_data: HashSet<ASMStatement>,

    funcs: HashMap<String, BuiltinFunc>,
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
                program.externs.push(ir!(extern, func_extern));
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
                    ir!(func_def, "print"),
                    ir!(func_call, "printf").asm,
                    ir!(func_ret),
                ],
                return_type: DataType::Void,
            },
        );
        checker.funcs.insert(
            String::from("println"),
            BuiltinFunc {
                externs: vec![String::from("printf")],
                data_sect: vec![ir!(data_str, "println_fmt", "%s\n")],
                text_sect: vec![
                    ir!(func_def, "println"),
                    ir!(mov, operand!(rsi, 8), operand!(rdi, 8)),
                    ir!(mov, operand!(rdi, 8), operand!(label, "println_fmt")),
                    ir!(func_call, "printf").asm,
                    ir!(func_ret),
                ],
                return_type: DataType::Void,
            },
        );
        //checker.funcs.insert(
        //    String::from("u64print"),
        //    BuiltinFunc {
        //        externs: vec![String::from("printf")],
        //        data_sect: vec![ir!(data_str, "u64print_fmt", "%llu\n")],
        //        text_sect: vec![ir!(include_str!("../builtin_fns/u64print.asm"))],
        //        return_type: DataType::Void,
        //    },
        //);
        //checker.funcs.insert(
        //    String::from("uadd"),
        //    BuiltinFunc {
        //        externs: Vec::new(),
        //        data_sect: Vec::new(),
        //        text_sect: vec![ir!(include_str!("../builtin_fns/uadd.asm"))],
        //        return_type: DataType::UInt64,
        //    },
        //);
        //checker.funcs.insert(
        //    String::from("usub"),
        //    BuiltinFunc {
        //        externs: Vec::new(),
        //        data_sect: Vec::new(),
        //        text_sect: vec![ir!(include_str!("../builtin_fns/usub.asm"))],
        //        return_type: DataType::UInt64,
        //    },
        //);
        //checker.funcs.insert(
        //    String::from("umul"),
        //    BuiltinFunc {
        //        externs: Vec::new(),
        //        data_sect: Vec::new(),
        //        text_sect: vec![ir!(include_str!("../builtin_fns/umul.asm"))],
        //        return_type: DataType::UInt64,
        //    },
        //);
        //checker.funcs.insert(
        //    String::from("udiv"),
        //    BuiltinFunc {
        //        externs: Vec::new(),
        //        data_sect: Vec::new(),
        //        text_sect: vec![ir!(include_str!("../builtin_fns/udiv.asm"))],
        //        return_type: DataType::UInt64,
        //    },
        //);
        checker
    }
}
