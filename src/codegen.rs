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
                    new.pop();
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

static FUNCTION_PROLOG: &str = "\tpush\trbp\n\n";
#[allow(dead_code)]
static FUNCTION_EPILOG: &str = "\n\tpop\trbp\n\tret\n";
fn func_epilog_ret(ret_value: u64) -> String {
    format!("\n\tpop\trbp\n\tmov\trax, {}\n\tret\n", ret_value)
}
macro_rules! code_call_func {
    ($name: expr) => {
        format!("\tcall\t_{}\n", $name).as_str()
    }
}
pub fn codegen(source: String) -> String {
    let raw_ast = ast::construct_ast(source);
    let mut ast = ast::AST::new();
    flatten_ast(&raw_ast, &mut raw_ast.iter(), &mut ast);

    let mut code_externs = String::new();
    let mut existing_externs: HashMap<&str, bool> = HashMap::new();
    let mut code_data_sect = String::from("\tsection .data\n");
    let mut code_text_sect = String::from("\tsection .text\n");
    for (i, node) in ast.iter().enumerate() {
        match &node.expr {
            ast::Expression::NumberLiteral(num) => {
                code_data_sect.push_str(format!("numliteral_{}:\t{}\n", i, num).as_str());
            }
            ast::Expression::StringLiteral(str) => {
                code_data_sect.push_str(format!("strliteral_{}:\tdb \"{}\", 0\n", i, str).as_str());
            }
            ast::Expression::FuncCall(name, _) => {
                if name == "print" && !existing_externs.contains_key("_printf") {
                    code_externs.push_str("\textern\t_printf\n");
                    existing_externs.insert("_printf", true);
                }
            }
            _ => {}
        }
    }

    // right now there's only one function main
    code_text_sect.push_str("\tglobal _main\n_main:\n");
    code_text_sect.push_str(FUNCTION_PROLOG);

    ast.iter().for_each(|node| match &node.expr {
        ast::Expression::FuncCall(name, args) => {
            // right now there's only one callable function print
            if name == "print" {
                code_text_sect.push_str(
                    format!(
                        "\tmov\trdi, strliteral_{}\n\tmov\trax, 0\n\tcall\t_printf\n",
                        args.first().unwrap_or_else(|| {
                            panic!("expected one string as argument of print function")
                        })
                    )
                    .as_str(),
                );
                return;
            }
            code_text_sect.push_str(code_call_func!(name));
        }
        ast::Expression::VarInitFunc(_, func_name, _) => {
            code_text_sect.push_str(code_call_func!(func_name));
        }
        _ => {}
    });

    //print_ast!(ast);
    code_text_sect.push_str(func_epilog_ret(0).as_str());

    format!("\n{}\n{}\n{}", code_externs, code_data_sect, code_text_sect)
}
