pub mod ast;
use std::collections::HashMap;

#[allow(unused_macros)]
macro_rules! print_ast {
    ($ast: expr) => {
        for (i, node) in $ast.iter().enumerate() {
            println!("{}: {:?}", i, node);
        }
    };
}

#[allow(unused, non_camel_case_types)]
#[derive(Clone, Debug)]
enum Register {
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
    fn name(&self) -> String {
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
#[allow(non_camel_case_types)]
type reg = Register;
#[derive(Debug)]
enum ASMStatement {
    SectionHead(String),

    Extern(String),
    VarInt(String, u64),
    VarStr(String, String),
    VarFromVar(String, String),
    VarSetFromReg(String, Register),

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
    fn gen_code(&self) -> String {
        let mut code = match self {
            Self::SectionHead(name) => format!("\n\tsection .{}", name),
            Self::Extern(name) => format!("\textern _{}", name),
            Self::VarInt(name, value) => format!("_{}:\tdq {}", name, value),
            Self::VarStr(name, value) => format!("_{}:\tdb {}", name, asm_fmt_str!(value)),
            Self::VarFromVar(name, rhs) => format!("_{}:\tequ _{}", name, rhs),
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
            Self::FuncRetVoid => format!("\tpop\trbp\n\tret\n"),
            Self::FuncRet(value) => format!("\tpop\trbp\n\tmov\trax, {}\n\tret\n", value),
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

struct ASMFuncCallConstructor {
    asm: ASMStatement,
}
impl ASMFuncCallConstructor {
    // pass argument by pointer
    fn arg_ptr(&mut self, arg: &ast::Expression) -> &mut ASMFuncCallConstructor {
        match &mut self.asm {
            ASMStatement::FuncCall(_, args) => args.push(match arg {
                ast::Expression::Identifier(name) => format!("_{}", name),
                ast::Expression::NumberLiteral(num) => format!("{}", num),
                ast::Expression::StringLiteral(str) => format!("{}", str),
                _ => panic!("unable to pass argument `{:?}` by reference", arg),
            }),
            _ => panic!("what the fuck"),
        }
        self
    }
    // pass argument by value
    fn arg_val(&mut self, arg: &ast::Expression) -> &mut ASMFuncCallConstructor {
        match &mut self.asm {
            ASMStatement::FuncCall(_, args) => args.push(match arg {
                ast::Expression::Identifier(name) => format!("[rel _{}]", name),
                ast::Expression::NumberLiteral(num) => format!("{}", num),
                _ => panic!("unable to pass argument `{:?}` by value", arg),
            }),
            _ => panic!("what the fuck"),
        }
        self
    }
}
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

static mut LAST_RAND: u64 = 0;
fn quick_rand(str: &str) -> u64 {
    let mut hash: u64 = 0;
    for ch in str.chars() {
        hash += (ch as u64) * (ch as u64);
    }
    unsafe {
        hash = hash.overflowing_add(LAST_RAND).0;
        LAST_RAND = hash;
    }
    hash
}

pub fn flatten_ast(old: &ast::AST, iter: &mut std::slice::Iter<ast::ASTNode>, new: &mut ast::AST) {
    let mut node: &ast::ASTNode;
    macro_rules! next {
        () => {
            match iter.next() {
                Some(x) => node = x,
                None => return,
            }
        };
    }
    loop {
        next!();
        if !node.is_recursive(old) {
            new.push(node.clone());
            continue;
        }
        match &node.expr {
            ast::Expression::FuncCall(name, args) => {
                let mut new_args: Vec<usize> = Vec::new();
                for arg in args {
                    if old.get(*arg).unwrap().is_recursive(old) {
                        flatten_ast(old, iter, new);
                    }
                    if !old.get(*arg).unwrap().is_recursive_type() {
                        new_args.push(*arg);
                        continue;
                    }
                    // the last added FuncCall statement won't be needed
                    new.last_mut().unwrap().expr = ast::Expression::Null;
                    // add a new VarInitFunc(...) before this FuncCall
                    let var_name = format!("temp_{}", quick_rand(name.as_str()));
                    new_args.push(new.len());
                    let func_name;
                    let func_args: &Vec<usize>;
                    match &old.get(*arg).unwrap().expr {
                        ast::Expression::FuncCall(name, args) => {
                            func_name = name;
                            func_args = args;
                        }
                        _ => panic!("unexpected variable initialization syntax"),
                    }
                    new.push(ast::ASTNode {
                        expr: ast::Expression::VarInitFunc(
                            var_name.clone(),
                            func_name.clone(),
                            func_args.clone(),
                        ),
                        parent: node.parent,
                    });
                }
                new.push(ast::ASTNode {
                    expr: ast::Expression::FuncCall(name.clone(), new_args),
                    parent: node.parent,
                });
            }
            ast::Expression::VarInit(lhs, rhs) => {
                new.last_mut().unwrap().expr = ast::Expression::Null;
                // should be a VarInitFunc
                let func_name;
                let func_args: &Vec<usize>;
                match &old.get(*rhs).unwrap().expr {
                    ast::Expression::FuncCall(name, args) => {
                        func_name = name;
                        func_args = args;
                    }
                    _ => panic!("unexpected variable initialization syntax"),
                }
                new.push(ast::ASTNode {
                    expr: ast::Expression::VarInitFunc(
                        lhs.clone(),
                        func_name.clone(),
                        func_args.clone(),
                    ),
                    parent: node.parent,
                });
            }
            _ => {}
        }
    }
}
fn add_builtin_fn_if_needed(
    name: &str,
    externs: &mut Vec<ASMStatement>,
    data_sect: &mut Vec<ASMStatement>,
    text_sect: &mut Vec<ASMStatement>,
    existed: &mut HashMap<&str, bool>,
) {
    match name {
        "print" | "print_int" => {
            if existed.contains_key("print") {
                return;
            }
            externs.push(asm!(extern, "printf"));
            data_sect.push(asm!(var_str, "printint_fmt", "%d\n"));
            existed.insert("print", true);
        }
        "add" => {
            if existed.contains_key("add") {
                return;
            }
            existed.insert("add", true);

            text_sect.push(asm!(func_def, "add"));
            text_sect.push(asm!(mov, reg::rax, reg::rdi));
            text_sect.push(asm!(add, reg::rax, reg::rsi));
            text_sect.push(asm!(func_ret));
        }
        "sub" => {
            if existed.contains_key("sub") {
                return;
            }
            existed.insert("sub", true);

            text_sect.push(asm!(func_def, "sub"));
            text_sect.push(asm!(mov, reg::rax, reg::rdi));
            text_sect.push(asm!(sub, reg::rax, reg::rsi));
            text_sect.push(asm!(func_ret));
        }
        "mul" => {
            if existed.contains_key("mul") {
                return;
            }
            existed.insert("mul", true);

            text_sect.push(asm!(func_def, "mul"));
            text_sect.push(asm!(mov, reg::rax, reg::rdi));
            text_sect.push(asm!(mul, reg::rsi));
            text_sect.push(asm!(func_ret));
        }
        "div" => {
            if existed.contains_key("div") {
                return;
            }
            existed.insert("div", true);

            text_sect.push(asm!(func_def, "div"));
            text_sect.push(asm!(mov, reg::rax, reg::rdi));
            text_sect.push(asm!(div, reg::rsi));
            text_sect.push(asm!(func_ret));
        }
        _ => {}
    }
}

pub fn codegen(source: String) -> String {
    let raw_ast = ast::construct_ast(source);
    let mut ast = ast::AST::new();
    flatten_ast(&raw_ast, &mut raw_ast.iter(), &mut ast);

    let mut existing_builtin_funcs: HashMap<&str, bool> = HashMap::new();

    // first generate data sections, add needed externs, and generate builtin functions in text
    // section
    let mut target_externs: Vec<ASMStatement> = Vec::new();
    let mut target_data_sect: Vec<ASMStatement> = vec![asm!(sect, "data")];
    let mut target_text_sect: Vec<ASMStatement> = vec![asm!(sect, "text")];
    for node in &ast {
        match &node.expr {
            ast::Expression::VarInitFunc(var_name, func_name, _) => {
                let asm = ASMStatement::VarInt(var_name.to_string(), 0);
                target_data_sect.push(asm);
                add_builtin_fn_if_needed(
                    func_name.as_str(),
                    &mut target_externs,
                    &mut target_data_sect,
                    &mut target_text_sect,
                    &mut existing_builtin_funcs,
                );
            }
            ast::Expression::FuncCall(name, _) => add_builtin_fn_if_needed(
                name.as_str(),
                &mut target_externs,
                &mut target_data_sect,
                &mut target_text_sect,
                &mut existing_builtin_funcs,
            ),
            ast::Expression::VarInit(name, rhs) => {
                let asm = match &ast.get(*rhs).unwrap().expr {
                    ast::Expression::NumberLiteral(num) => asm!(var_int, name, *num),
                    ast::Expression::Identifier(name_2) => asm!(var_equ, name, name_2),
                    _ => panic!("Invalid rhs for `let`"),
                };
                target_data_sect.push(asm);
            }
            _ => {}
        }
    }

    // then generate text section
    target_text_sect.push(asm!(func_def, "main"));
    for node in &ast {
        match &node.expr {
            ast::Expression::FuncCall(name, args) => {
                let mut func_call: ASMFuncCallConstructor =
                    if name == "print" || name == "print_int" {
                        asm!(func_call, &"printf")
                    } else {
                        asm!(func_call, &"printf")
                    };
                if name == "print_int" {
                    func_call.arg_ptr(&ast::Expression::Identifier("printint_fmt".to_string()));
                }
                args.iter().for_each(|arg_i| {
                    let arg = &ast.get(*arg_i).unwrap().expr;
                    func_call.arg_val(arg);
                });
                target_text_sect.push(func_call.asm);
            }
            ast::Expression::VarInitFunc(var_name, func_name, args) => {
                let mut func_call = asm!(func_call, func_name);
                args.iter().for_each(|arg_i| {
                    let arg = &ast.get(*arg_i).unwrap().expr;
                    func_call.arg_val(arg);
                });
                target_text_sect.push(func_call.asm);
                let var_set = asm!(var_set_reg, var_name, reg::rax);
                target_text_sect.push(var_set);
            }
            _ => {}
        }
    }
    target_text_sect.push(asm!(func_ret, 0));

    let mut full_generated_code = String::new();
    for statement in &target_externs {
        full_generated_code.push_str(statement.gen_code().as_str());
    }
    for statement in &target_data_sect {
        full_generated_code.push_str(statement.gen_code().as_str());
    }
    for statement in &target_text_sect {
        full_generated_code.push_str(statement.gen_code().as_str());
    }
    full_generated_code
}
