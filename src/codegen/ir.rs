// ir: intermedia representation

use std::collections::HashMap;

// names of the register to pass function calling arguments to
static ARG_REG_NAMES: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

#[macro_export]
macro_rules! ir {
    (sect, $sect_name: expr) => {
        ASMStatement::SectionHead(String::from($sect_name))
    };
    (label, $label_name: expr) => {
        ASMStatement::Label(String::from($label_name))
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
    pub strliterals_ids: HashMap<String, u64>,
}
impl Program {
    pub fn new() -> Program {
        Program {
            externs: Vec::new(),
            data_sect: Vec::new(),
            funcs: Vec::new(),
            strliterals_ids: HashMap::new(),
        }
    }
    pub fn description(&self) -> String {
        let mut result = String::new();
        for asm in &self.externs {
            result.push_str(&asm.description());
        }
        for asm in &self.data_sect {
            result.push_str(&asm.description());
        }
        result.push_str(&ir!(sect, "text").description());
        for func in &self.funcs {
            for asm in func {
                result.push_str(&asm.description());
            }
        }
        result
    }
    pub fn gen_code(&self) -> String {
        let mut result = String::new();
        for asm in &self.externs {
            result.push_str(&asm.gen_code());
        }
        for asm in &self.data_sect {
            result.push_str(&asm.gen_code());
        }
        result.push_str(&ir!(sect, "text").gen_code());
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

    eax,
    ebx,
    ecx,
    edx,
    esi,
    edi,
    esp,
    ebp,
    r8d,
    r9d,
    r10d,
    r11d,
    r12d,
    r13d,
    r14d,
    r15d,

    ax,
    bx,
    cx,
    dx,
    si,
    di,
    sp,
    bp,
    r8w,
    r9w,
    r10w,
    r11w,
    r12w,
    r13w,
    r14w,
    r15w,
}
impl Register {
    pub fn name(&self) -> String {
        match self {
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

            Self::eax => String::from("eax"),
            Self::ebx => String::from("ebx"),
            Self::ecx => String::from("ecx"),
            Self::edx => String::from("edx"),
            Self::esi => String::from("esi"),
            Self::edi => String::from("edi"),
            Self::esp => String::from("esp"),
            Self::ebp => String::from("ebp"),
            Self::r8d => String::from("r8d"),
            Self::r9d => String::from("r9d"),
            Self::r10d => String::from("r10d"),
            Self::r11d => String::from("r11d"),
            Self::r12d => String::from("r12d"),
            Self::r13d => String::from("r13d"),
            Self::r14d => String::from("r14d"),
            Self::r15d => String::from("r15d"),

            Self::ax => String::from("ax"),
            Self::bx => String::from("bx"),
            Self::cx => String::from("cx"),
            Self::dx => String::from("dx"),
            Self::si => String::from("si"),
            Self::di => String::from("di"),
            Self::sp => String::from("sp"),
            Self::bp => String::from("bp"),
            Self::r8w => String::from("r8w"),
            Self::r9w => String::from("r9w"),
            Self::r10w => String::from("r10w"),
            Self::r11w => String::from("r11w"),
            Self::r12w => String::from("r12w"),
            Self::r13w => String::from("r13w"),
            Self::r14w => String::from("r14w"),
            Self::r15w => String::from("r15w"),
        }
    }
    pub fn _64bit(self) -> Register {
        match self {
            Self::rax => Self::rax,
            Self::rbx => Self::rbx,
            Self::rcx => Self::rcx,
            Self::rdx => Self::rdx,
            Self::rsi => Self::rsi,
            Self::rdi => Self::rdi,
            Self::rsp => Self::rsp,
            Self::rbp => Self::rbp,
            Self::r8 => Self::r8,
            Self::r9 => Self::r9,
            Self::r10 => Self::r10,
            Self::r11 => Self::r11,
            Self::r12 => Self::r12,
            Self::r13 => Self::r13,
            Self::r14 => Self::r14,
            Self::r15 => Self::r15,

            Self::eax => Self::rax,
            Self::ebx => Self::rbx,
            Self::ecx => Self::rcx,
            Self::edx => Self::rdx,
            Self::esi => Self::rsi,
            Self::edi => Self::rdi,
            Self::esp => Self::rsp,
            Self::ebp => Self::rbp,
            Self::r8d => Self::r8,
            Self::r9d => Self::r9,
            Self::r10d => Self::r10,
            Self::r11d => Self::r11,
            Self::r12d => Self::r12,
            Self::r13d => Self::r13,
            Self::r14d => Self::r14,
            Self::r15d => Self::r15,

            Self::ax => Self::rax,
            Self::bx => Self::rbx,
            Self::cx => Self::rcx,
            Self::dx => Self::rdx,
            Self::si => Self::rsi,
            Self::di => Self::rdi,
            Self::sp => Self::rsp,
            Self::bp => Self::rbp,
            Self::r8w => Self::r8,
            Self::r9w => Self::r9,
            Self::r10w => Self::r10,
            Self::r11w => Self::r11,
            Self::r12w => Self::r12,
            Self::r13w => Self::r13,
            Self::r14w => Self::r14,
            Self::r15w => Self::r15,
        }
    }
    pub fn _32bit(self) -> Register {
        match self {
            Self::rax => Self::eax,
            Self::rbx => Self::ebx,
            Self::rcx => Self::ecx,
            Self::rdx => Self::edx,
            Self::rsi => Self::esi,
            Self::rdi => Self::edi,
            Self::rsp => Self::esp,
            Self::rbp => Self::ebp,
            Self::r8 => Self::r8d,
            Self::r9 => Self::r9d,
            Self::r10 => Self::r10d,
            Self::r11 => Self::r11d,
            Self::r12 => Self::r12d,
            Self::r13 => Self::r13d,
            Self::r14 => Self::r14d,
            Self::r15 => Self::r15d,

            Self::eax => Self::eax,
            Self::ebx => Self::ebx,
            Self::ecx => Self::ecx,
            Self::edx => Self::edx,
            Self::esi => Self::esi,
            Self::edi => Self::edi,
            Self::esp => Self::esp,
            Self::ebp => Self::ebp,
            Self::r8d => Self::r8d,
            Self::r9d => Self::r9d,
            Self::r10d => Self::r10d,
            Self::r11d => Self::r11d,
            Self::r12d => Self::r12d,
            Self::r13d => Self::r13d,
            Self::r14d => Self::r14d,
            Self::r15d => Self::r15d,

            Self::ax => Self::eax,
            Self::bx => Self::ebx,
            Self::cx => Self::ecx,
            Self::dx => Self::edx,
            Self::si => Self::esi,
            Self::di => Self::edi,
            Self::sp => Self::esp,
            Self::bp => Self::ebp,
            Self::r8w => Self::r8d,
            Self::r9w => Self::r9d,
            Self::r10w => Self::r10d,
            Self::r11w => Self::r11d,
            Self::r12w => Self::r12d,
            Self::r13w => Self::r13d,
            Self::r14w => Self::r14d,
            Self::r15w => Self::r15d,
        }
    }
    pub fn _16bit(self) -> Register {
        match self {
            Self::rax => Self::ax,
            Self::rbx => Self::bx,
            Self::rcx => Self::cx,
            Self::rdx => Self::dx,
            Self::rsi => Self::si,
            Self::rdi => Self::di,
            Self::rsp => Self::sp,
            Self::rbp => Self::bp,
            Self::r8 => Self::r8w,
            Self::r9 => Self::r9w,
            Self::r10 => Self::r10w,
            Self::r11 => Self::r11w,
            Self::r12 => Self::r12w,
            Self::r13 => Self::r13w,
            Self::r14 => Self::r14w,
            Self::r15 => Self::r15w,

            Self::eax => Self::ax,
            Self::ebx => Self::bx,
            Self::ecx => Self::cx,
            Self::edx => Self::dx,
            Self::esi => Self::si,
            Self::edi => Self::di,
            Self::esp => Self::sp,
            Self::ebp => Self::bp,
            Self::r8d => Self::r8w,
            Self::r9d => Self::r9w,
            Self::r10d => Self::r10w,
            Self::r11d => Self::r11w,
            Self::r12d => Self::r12w,
            Self::r13d => Self::r13w,
            Self::r14d => Self::r14w,
            Self::r15d => Self::r15w,

            Self::ax => Self::ax,
            Self::bx => Self::bx,
            Self::cx => Self::cx,
            Self::dx => Self::dx,
            Self::si => Self::si,
            Self::di => Self::di,
            Self::sp => Self::sp,
            Self::bp => Self::bp,
            Self::r8w => Self::r8w,
            Self::r9w => Self::r9w,
            Self::r10w => Self::r10w,
            Self::r11w => Self::r11w,
            Self::r12w => Self::r12w,
            Self::r13w => Self::r13w,
            Self::r14w => Self::r14w,
            Self::r15w => Self::r15w,
        }
    }
    pub fn of_size(self, size: usize) -> Register {
        match size {
            8 => self._64bit(),
            4 => self._32bit(),
            2 => self._16bit(),
            1 => todo!(),
            _ => panic!(),
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OperandContent {
    Reg(Register),     // rax, rbx, ...
    StaticVar(String), // [rel _label]
    LocalVar(u64),     // qword [rbp - addr]
    Label(String),     // _label
    Int(u64),          //
    Raw(String),
}
#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AddrMode {
    Direct,
    Indirect,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Operand {
    pub len: usize, // 8, 4, 2, 1
    pub addr_mode: AddrMode,
    pub content: OperandContent,
}
impl Operand {
    pub fn text(&self) -> String {
        match self.addr_mode {
            AddrMode::Direct => {
                match &self.content {
                    OperandContent::Reg(reg) => match self.len {
                        8 => reg._64bit(),
                        4 => reg._32bit(),
                        2 => reg._16bit(),
                        //1 => reg._8bit().name(),
                        _ => panic!(),
                    }
                    .name(),
                    OperandContent::StaticVar(name) => format!("_{}", name),
                    OperandContent::LocalVar(var_addr) => format!("{} [rbp - {}]", match self.len {
                        8 => "qword",
                        4 => "dword",
                        2 => "bword",
                        1 => "word",
                        _ => panic!(),
                    }, var_addr),
                    OperandContent::Label(name) => format!("{}", name),
                    OperandContent::Int(number) => format!("{}", number),
                    OperandContent::Raw(code) => format!(
                        "{}{}",
                        code,
                        if code.chars().last().unwrap() == '\n' {
                            ""
                        } else {
                            "\n"
                        }
                    ),
                }
            }
            AddrMode::Indirect => todo!(),
        }
    }
    pub fn description(&self) -> String {
        match &self.content {
            OperandContent::Reg(reg) => reg.name(),
            OperandContent::StaticVar(name) => format!("static var {}", name),
            OperandContent::LocalVar(var_addr) => format!("local var {}", var_addr),
            OperandContent::Label(name) => format!("label{}", name),
            OperandContent::Int(number) => format!("{}", number),
            OperandContent::Raw(code) => format!("raw {:?}", code),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum ASMStatement {
    SectionHead(String),

    Extern(String),

    Label(String),

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
    pub fn description(&self) -> String {
        match self {
            Self::SectionHead(name) => format!("section\t{}\n", name),
            Self::Label(name) => format!("label\t{}\n", name),
            Self::Extern(name) => format!("extern\t_{}\n", name),
            Self::DataInt(name, value) => format!("data\t{}\t{}\n", name, value),
            Self::DataStr(name, value) => format!("data\t{}\t{:?}\n", name, value),
            Self::Mov(oper0, oper1) => {
                format!("mov\t{}, {}\n", oper0.description(), oper1.description())
            }
            Self::FuncCall(name, args) => {
                let mut result = format!("call\t{}", name);
                for arg in args {
                    result.push_str(format!("\n\t{}", arg.description()).as_str());
                }
                result.push('\n');
                result
            }
            Self::FuncDef(name) => format!("fndef\t{}\n", name),
            Self::FuncRetVoid => format!("return\n"),
            Self::FuncRet(oper) => format!("ret\t{}\n", oper.description()),
            Self::Add(oper0, oper1) => {
                format!("add\t{}, {}\n", oper0.description(), oper1.description())
            }
            Self::Sub(oper0, oper1) => {
                format!("sub\t{}, {}\n", oper0.description(), oper1.description())
            }
            Self::Mul(oper) => format!("mul\t{}\n", oper.description()),
            Self::Div(oper) => format!("div\t{}\n", oper.description()),
            Self::Raw(code) => format!("raw\t{}", code),
        }
    }
    pub fn gen_code(&self) -> String {
        let mut code = match self {
            Self::SectionHead(name) => format!("\n\tsection .{}", name),
            Self::Label(name) => format!("{}:", name),
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
            Self::FuncDef(name) => format!(
                "\n\tglobal _{}\n_{}:\n\tpush\trbp\n\tmov\trbp, rsp\n",
                name, name
            ),
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
macro_rules! operand {
    (label, $l: expr, $x: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Label($x),
        }
    };
    (int, $l: expr, $x: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Int($x),
        }
    };
    (var, $l: expr,$x: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::LocalVar($x),
        }
    };
    (var, ptr, $l: expr, $x: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Indirect,
            content: OperandContent::LocalVar($x),
        }
    };
    (rax, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::rax.of_size($l as usize)),
        }
    };
    (rbx, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::rbx.of_size($l as usize)),
        }
    };
    (rcx, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::rcx.of_size($l as usize)),
        }
    };
    (rdx, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::rdx.of_size($l as usize)),
        }
    };
    (rsi, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::rsi.of_size($l as usize)),
        }
    };
    (rdi, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::rdi.of_size($l as usize)),
        }
    };
    (rsp, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::rsp.of_size($l as usize)),
        }
    };
    (rbp, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::rbp.of_size($l as usize)),
        }
    };
    (r8, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::r8.of_size($l as usize)),
        }
    };
    (r9, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::r9.of_size($l as usize)),
        }
    };
    (r10, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::r10.of_size($l as usize)),
        }
    };
    (r11, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::r11.of_size($l as usize)),
        }
    };
    (r12, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::r12.of_size($l as usize)),
        }
    };
    (r13, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::r13.of_size($l as usize)),
        }
    };
    (r14, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::r14.of_size($l as usize)),
        }
    };
    (r15, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg(Register::r15.of_size($l as usize)),
        }
    };
    (reg, $reg: expr, $l: expr) => {
        Operand {
            len: $l as usize,
            addr_mode: AddrMode::Direct,
            content: OperandContent::Reg($reg.of_size($l as usize)),
        }
    };
}
