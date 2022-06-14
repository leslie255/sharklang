// ir: intermedia representation
use super::ast::Expression;

#[allow(unused, non_camel_case_types)]
#[derive(Clone, Debug)]
pub enum Register {
    rax,
    rbx,
    rcx,
    rdx,
    rsi,
    rdi,
    rsp,
    rbp,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
}
impl Register {
    pub fn name(&self) -> String {
        match &self {
            Self::rax => String::from("rax"),
            Self::rbx => String::from("rbx"),
            Self::rcx => String::from("rcx"),
            Self::rdx => String::from("rdx"),
            Self::rsi => String::from("rsi"),
            Self::rdi => String::from("rdi"),
            Self::rsp => String::from("rsp"),
            Self::rbp => String::from("rbp"),
            Self::r8 => String::from("r8"),
            Self::r9 => String::from("r9"),
            Self::r10 => String::from("r10"),
            Self::r11 => String::from("r11"),
            Self::r12 => String::from("r12"),
            Self::r13 => String::from("r13"),
            Self::r14 => String::from("r14"),
            Self::r15 => String::from("r15"),
        }
    }
}
#[derive(Debug)]
#[allow(dead_code)]
pub enum ASMStatement {
    SectionHead(String),

    Extern(String),
    VarInt(String, u64),
    VarStr(String, String),
    VarFromVar(String, String),
    VarSetFromReg(String, Register),
    VarSetInt(String, u64),
    VarSetStr(String, String),

    FuncCall(String, Vec<String>), // function name, should pass as value/reference, argument

    FuncDef(String),
    FuncRetVoid,
    FuncRet(u64),

    MovRegReg(Register, Register),
    Add(Register, Register),
    Sub(Register, Register),
    Mul(Register),
    Div(Register),
}
macro_rules! asm_fmt_str {
    // converts '\n' into 0x0A, etc.
    ($string: expr) => {{
        let mut result = String::from("\"");
        let mut is_outside_quote = false;
        for ch in $string.chars() {
            if ch == '\n' {
                result.push_str("\", 0x0A");
                is_outside_quote = true;
                continue;
            }
            if is_outside_quote {
                result.push_str("\", ");
                is_outside_quote = false;
            }
            result.push(ch);
        }
        if !is_outside_quote {
            result.push('\"');
        }
        result.push_str(", 0");
        result
    }};
}
// names of the register to pass function calling arguments to
static ARG_REG_NAMES: [&'static str; 3] = ["rdi", "rsi", "rbp"];
impl ASMStatement {
    pub fn gen_code(&self) -> String {
        let mut code = match self {
            Self::SectionHead(name) => format!("\n\tsection .{}", name),
            Self::Extern(name) => format!("\textern _{}", name),
            Self::VarInt(name, value) => format!("_{}:\tdq {}", name, value),
            Self::VarStr(name, value) => format!("_{}:\tdb {}", name, asm_fmt_str!(value)),
            Self::VarFromVar(name, rhs) => format!("_{}:\tequ _{}", name, rhs),
            Self::VarSetInt(name, rhs) => format!("\tmov\t[rel _{}], {}", name, rhs),
            Self::VarSetStr(name, rhs) => format!("\tmov\t_{}, _{}", name, rhs),
            Self::FuncCall(name, args) => {
                let mut result = String::new();
                args.iter().enumerate().for_each(|(i, arg)| {
                    result.push_str(format!("\tmov\t{}, {}\n", ARG_REG_NAMES[i], arg).as_str());
                });
                result.push_str(format!("\tcall\t_{}\n", name).as_str());
                result
            }
            ASMStatement::VarSetFromReg(name, reg) => {
                format!("\tmov\t[rel _{}], {}", name, reg.name())
            }
            Self::FuncDef(name) => format!("\n\tglobal _{}\n_{}:\n\tpush\trbp\n", name, name),
            Self::FuncRetVoid => format!("\tpop\trbp\n\tret"),
            Self::FuncRet(value) => format!("\tpop\trbp\n\tmov\trax, {}\n\tret", value),
            Self::MovRegReg(reg0, reg1) => format!("\tmov\t{}, {}", reg0.name(), reg1.name()),
            Self::Add(reg0, reg1) => format!("\tadd\t{}, {}", reg0.name(), reg1.name()),
            Self::Sub(reg0, reg1) => format!("\tsub\t{}, {}", reg0.name(), reg1.name()),
            Self::Mul(reg0) => format!("\tmul\t{}", reg0.name()),
            Self::Div(reg0) => format!("\tdiv\t{}", reg0.name()),
        };
        code.push_str("\n");
        code
    }
}

pub struct ASMFuncCallConstructor {
    pub asm: ASMStatement,
}
impl ASMFuncCallConstructor {
    // pass argument by pointer
    pub fn arg_ptr(&mut self, arg: &Expression) -> &mut ASMFuncCallConstructor {
        match &mut self.asm {
            ASMStatement::FuncCall(_, args) => args.push(match arg {
                Expression::Identifier(name) => format!("_{}", name),
                Expression::NumberLiteral(num) => format!("{}", num),
                Expression::StringLiteral(str) => format!("{}", str),
                _ => panic!("unable to pass argument `{:?}` by reference", arg),
            }),
            _ => panic!("what the fuck"),
        }
        self
    }
    // pass argument by value
    pub fn arg_val(&mut self, arg: &Expression) -> &mut ASMFuncCallConstructor {
        match &mut self.asm {
            ASMStatement::FuncCall(_, args) => args.push(match arg {
                Expression::Identifier(name) => format!("[rel _{}]", name),
                Expression::NumberLiteral(num) => format!("{}", num),
                _ => panic!("unable to pass argument `{:?}` by value", arg),
            }),
            _ => panic!("what the fuck"),
        }
        self
    }
}
#[macro_export]
macro_rules! asm {
    (sect, $sect_name: expr) => {
        ASMStatement::SectionHead(String::from($sect_name))
    };
    (var_int, $name: expr, $value: expr) => {
        ASMStatement::VarInt($name.to_string(), $value)
    };
    (var_str, $name: expr, $value: expr) => {
        ASMStatement::VarStr($name.to_string(), $value.to_string())
    };
    (var_equ, $name: expr, $rhs: expr) => {
        ASMStatement::VarFromVar($name.to_string(), $rhs.to_string())
    };
    (var_set_reg, $var_name: expr, $reg: expr) => {
        ASMStatement::VarSetFromReg($var_name.to_string(), $reg)
    };
    (var_set_str, $var_name: expr, $str: expr) => {
        ASMStatement::VarSetStr($var_name.to_string(), $str.to_string())
    };
    (var_set_int, $var_name: expr, $num: expr) => {
        ASMStatement::VarSetInt($var_name.to_string(), $num)
    };
    (extern, $name: expr) => {
        ASMStatement::Extern($name.to_string())
    };
    (func_call, $name: expr) => {
        ASMFuncCallConstructor {
            asm: ASMStatement::FuncCall($name.to_string(), Vec::new()),
        }
    };
    (func_ret) => {
        ASMStatement::FuncRetVoid
    };
    (func_ret, $value: expr) => {
        ASMStatement::FuncRet($value)
    };
    (func_def, $name: expr) => {
        ASMStatement::FuncDef($name.to_string())
    };
    (mov, $reg0: expr, $reg1: expr) => {
        ASMStatement::MovRegReg($reg0, $reg1)
    };
    (add, $reg0: expr, $reg1: expr) => {
        ASMStatement::Add($reg0, $reg1)
    };
    (sub, $reg0: expr, $reg1: expr) => {
        ASMStatement::Sub($reg0, $reg1)
    };
    (mul, $reg: expr) => {
        ASMStatement::Mul($reg)
    };
    (div, $reg: expr) => {
        ASMStatement::Div($reg)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rax {
    () => {
        Register::rax
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rbx {
    () => {
        Register::rbx
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rcx {
    () => {
        Register::rcx
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rsi {
    () => {
        Register::rsi
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rdi {
    () => {
        Register::rdi
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rsp {
    () => {
        Register::rsp
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rbp {
    () => {
        Register::rbp
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r8 {
    () => {
        Register::r8
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r9 {
    () => {
        Register::r9
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r10 {
    () => {
        Register::r10
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r11 {
    () => {
        Register::r11
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r12 {
    () => {
        Register::r12
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r13 {
    () => {
        Register::r13
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r14 {
    () => {
        Register::r14
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r15 {
    () => {
        Register::r15
    };
}
