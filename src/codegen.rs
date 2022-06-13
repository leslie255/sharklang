#![allow(unused)]
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

#[allow(unused)]
#[derive(Clone)]
enum ASMStatement {
    SectionHead(String),

    Extern(String),
    VarInt(String, u64),
    VarStr(String, String),
    VarFromVar(String, String),

    FuncCall(String, Vec<String>), // function name, should pass as value/reference, argument
    VarSetFromFunc(String, String),

    FuncDef(String),
    FuncRetVoid,
    FuncRet(u64),
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
    (var_equ, $name: expr, $rhs: expr) => {
        ASMStatement::VarFromVar($name.to_string(), $rhs.to_string())
    };
    (func_call, $name: expr) => {
        ASMFuncCallConstructor {
            asm: ASMStatement::FuncCall($name.to_string(), Vec::new()),
        }
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
// names of the register to pass function calling arguments to
static ARG_REG_NAMES: [&'static str; 3] = ["rdi", "rsi", "rbp"];
impl ASMStatement {
    fn gen_code(&self) -> String {
        let mut code = match self {
            Self::SectionHead(name) => format!("\tsection .{}", name),
            Self::Extern(name) => format!("\textern _{}", name),
            Self::VarInt(name, value) => format!("_{}:\tdq {}", name, value),
            Self::VarStr(name, value) => format!("_{}:\tdb {}", name, asm_fmt_str!(value)),
            Self::VarFromVar(name, rhs) => format!("_{}:\tequ _{}", name, rhs),
            Self::FuncCall(name, args) => {
                let mut result = String::new();
                args.iter().enumerate().for_each(|(i, arg)| {
                    result.push_str(format!("\tmov\t{}, {}\n", ARG_REG_NAMES[i], arg).as_str());
                });
                result.push_str(format!("\tcall\t_{}", name).as_str());
                result
            }
            Self::VarSetFromFunc(func_name, var_name) => {
                format!("\tcall _{}\n\tmov\t[rel _{}], rax", func_name, var_name)
            }
            Self::FuncDef(name) => format!("\tglobal _{}\n_{}:\n\tpush\trbp", name, name),
            Self::FuncRetVoid => format!("\tpop\trbp\n\tret"),
            Self::FuncRet(value) => format!("\tpop\trbp\n\tmov\trax, {}\n\tret", value),
        };
        code.push_str("\n\n");
        code
    }
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

pub fn codegen(source: String) -> String {
    let raw_ast = ast::construct_ast(source);
    let mut ast = ast::AST::new();
    flatten_ast(&raw_ast, &mut raw_ast.iter(), &mut ast);

    let mut existing_builtin_funcs: HashMap<&str, bool> = HashMap::new();

    // first generate data sections and needed externs
    let mut target_externs: Vec<ASMStatement> = Vec::new();
    let mut target_data_sect: Vec<ASMStatement> = vec![asm!(sect, "data")];
    for node in &ast {
        match &node.expr {
            ast::Expression::FuncCall(name, _) | ast::Expression::VarInitFunc(_, name, _) => {
                match name.as_str() {
                    "print" | "print_int" => {
                        if existing_builtin_funcs.contains_key("print") {
                            continue;
                        }
                        target_externs.push(ASMStatement::Extern("printf".to_string()));
                        target_data_sect.push(ASMStatement::VarStr(
                            "printint_fmt".to_string(),
                            "%d\n".to_string(),
                        ));
                        existing_builtin_funcs.insert("print", true);
                    }
                    _ => {}
                }
            }
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
    let mut target_text_sect: Vec<ASMStatement> = vec![asm!(sect, "text")];
    target_text_sect.push(ASMStatement::FuncDef("main".to_string()));
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
            _ => {}
        }
    }
    target_text_sect.push(ASMStatement::FuncRet(0));

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
