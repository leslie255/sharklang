// ir: intermedia representation
#[macro_export]
macro_rules! asm {
    (sect, $sect_name: expr) => {
        ASMStatement::SectionHead(String::from($sect_name))
    };
    (extern, $name: expr) => {
        ASMStatement::Extern($name.to_string())
    };
    (data_int, $name: expr, $data: expr) => {
        ASMStatement::DataInt($name.to_string(), $data)
    };
    (data_str, $name: expr, $data: expr) => {
        ASMStatement::DataStr($name.to_string(), $data.to_string())
    };
    (mov, $oper0: expr, $oper1: expr) => {
        ASMStatement::Mov($oper0, $oper1)
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
    (add, $reg: expr, $oper: expr) => {
        ASMStatement::Add($reg, $oper)
    };
    (sub, $reg: expr, $oper: expr) => {
        ASMStatement::Sub($reg, $oper)
    };
    (mul, $oper: expr) => {
        ASMStatement::Mul($oper)
    };
    (div, $oper: expr) => {
        ASMStatement::Div($oper)
    };
    ($code: expr) => {
        ASMStatement::Raw($code.to_string())
    };
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

#[derive(Debug, Clone)]
pub struct Program {
    pub externs: Vec<ASMStatement>,
    pub data_sect: Vec<ASMStatement>,
    pub funcs: Vec<Vec<ASMStatement>>,
}
impl Program {
    pub fn new() -> Program {
        Program {
            externs: Vec::new(),
            data_sect: vec![asm!(sect, "data"), asm!(data_int, "temp", 0)],
            funcs: Vec::new(),
        }
    }
    pub fn gen_code(&self) -> String {
        let mut result = String::new();
        for asm in &self.externs {
            result.push_str(&asm.gen_code());
        }
        for asm in &self.data_sect {
            result.push_str(&asm.gen_code());
        }
        result.push_str(&asm!(sect, "text").gen_code());
        for func in &self.funcs {
            for asm in func {
                result.push_str(&asm.gen_code());
            }
        }
        result
    }
}

#[allow(unused, non_camel_case_types)]
#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
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
#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Reg(Register), // rax, rbx, ...
    Var(String),   // [rel _label]
    Addr(String),  // _label
    Int(u64),      // just data
}
impl Operand {
    pub fn text(&self) -> String {
        match self {
            Self::Reg(reg) => reg.name(),
            Self::Var(name) => format!("[rel _{}]", name),
            Self::Addr(addr) => format!("_{}", addr),
            Self::Int(int) => format!("{}", int),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum ASMStatement {
    SectionHead(String),

    Extern(String),

    DataInt(String, u64),
    DataStr(String, String),

    Mov(Operand, Operand),

    FuncCall(String, Vec<Operand>),

    FuncDef(String),
    FuncRetVoid,
    FuncRet(Operand),

    Add(Operand, Operand),
    Sub(Operand, Operand),
    Mul(Operand),
    Div(Operand),

    Raw(String),
}
impl ASMStatement {
    pub fn gen_code(&self) -> String {
        let mut code = match self {
            Self::SectionHead(name) => format!("\n\tsection .{}", name),
            Self::Extern(name) => format!("\textern _{}", name),
            Self::DataInt(name, value) => format!("_{}:\tdq {}", name, value),
            Self::DataStr(name, value) => format!("_{}:\tdb {}", name, asm_fmt_str!(value)),
            Self::Mov(oper0, oper1) => format!("\tmov\t{}, {}", oper0.text(), oper1.text()),
            Self::FuncCall(name, args) => {
                let mut result = String::new();
                args.iter().enumerate().for_each(|(i, arg)| {
                    result.push_str(
                        format!("\tmov\t{}, {}\n", ARG_REG_NAMES[i], arg.text()).as_str(),
                    );
                });
                result.push_str(format!("\tcall\t_{}\n", name).as_str());
                result
            }
            Self::FuncDef(name) => format!("\n\tglobal _{}\n_{}:\n\tpush\trbp\n", name, name),
            Self::FuncRetVoid => format!("\tpop\trbp\n\tret"),
            Self::FuncRet(oper) => format!("\tpop\trbp\n\tmov\trax, {}\n\tret", oper.text()),
            Self::Add(oper0, oper1) => format!("\tadd\t{}, {}", oper0.text(), oper1.text()),
            Self::Sub(oper0, oper1) => format!("\tsub\t{}, {}", oper0.text(), oper1.text()),
            Self::Mul(oper) => format!("\tmul\t{}", oper.text()),
            Self::Div(oper) => format!("\tdiv\t{}", oper.text()),
            Self::Raw(code) => code.clone(),
        };
        code.push_str("\n");
        code
    }
}
pub struct ASMFuncCallConstructor {
    pub asm: ASMStatement,
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
// names of the register to pass function calling arguments to
static ARG_REG_NAMES: [&'static str; 2] = ["rdi", "rsi"];

impl ASMFuncCallConstructor {
    pub fn arg(&mut self, arg: Operand) -> &mut ASMFuncCallConstructor {
        match &mut self.asm {
            ASMStatement::FuncCall(_, args) => args.push(arg),
            _ => panic!("what the fuck"),
        }
        self
    }
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! addr {
    ($name: expr) => {
        Operand::Addr($name.to_string())
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rax {
    () => {
        Operand::Reg(Register::rax)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rbx {
    () => {
        Operand::Reg(Register::rbx)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rcx {
    () => {
        Operand::Reg(Register::rcx)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rsi {
    () => {
        Operand::Reg(Register::rsi)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rdi {
    () => {
        Operand::Reg(Register::rdi)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rsp {
    () => {
        Operand::Reg(Register::rsp)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! rbp {
    () => {
        Operand::Reg(Register::rbp)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r8 {
    () => {
        Operand::Reg(Register::r8)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r9 {
    () => {
        Operand::Reg(Register::r9)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r10 {
    () => {
        Operand::Reg(Register::r10)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r11 {
    () => {
        Operand::Reg(Register::r11)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r12 {
    () => {
        Operand::Reg(Register::r12)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r13 {
    () => {
        Operand::Reg(Register::r13)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r14 {
    () => {
        Operand::Reg(Register::r14)
    };
}
#[allow(unused_macros)]
#[macro_export]
macro_rules! r15 {
    () => {
        Operand::Reg(Register::r15)
    };
}
