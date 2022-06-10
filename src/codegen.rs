#![allow(unused)] // temporary
mod ast;
use std::collections::HashMap;

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

static FUNCTION_PROLOG: &str = "\tpush\trbp\n\n";
static FUNCTION_EPILOG: &str = "\n\tpop\trbp\n\tret\n";
fn func_epilog_ret(ret_value: u64) -> String {
    format!("\n\tpop\trbp\n\tmov\trax, {}\n\tret\n", ret_value)
}
pub fn codegen(source: String) -> String {
    let ast = ast::construct_ast(source);

    let mut code_externs = String::new();
    let mut existing_externs: HashMap<&str, bool> = HashMap::new();
    let mut code_data_sect = String::from("\tsection .data\n");
    let mut code_text_sect = String::from("\tsection .text\n");
    ast.iter()
        .enumerate()
        .for_each(|(i, node)| match &node.expr {
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
        });
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
            }
        }
        _ => {}
    });

    code_text_sect.push_str(func_epilog_ret(0).as_str());

    format!(
        "\n{}\n{}\n{}",
        code_externs, code_data_sect, code_text_sect
    )
}
