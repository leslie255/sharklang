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

#[allow(unused_variables)]
fn flatten_ast(ast: &mut ast::AST) {
    // TODO
}

fn data_sect_gen_literal(
    map: &mut HashMap<String, String>,
    expr: &ast::Expression,
    code: &mut String,
) {
    match expr {
        ast::Expression::StringLiteral(str) => {
            let rand = quick_rand(str);
            code.push_str(format!("strliteral_{}:\tdb \"{}\", 0\n", rand, str).as_str());
            map.insert(str.to_string(), format!("strliteral_{}", rand));
        }
        ast::Expression::NumberLiteral(num) => {
            let rand = quick_rand(stringify!(num));
            code.push_str(format!("numliteral_{}:\tdb {}\n", rand, num).as_str());
            map.insert(num.to_string(), format!("numliteral_{}", rand));
        }
        _ => {}
    }
}

pub fn codegen(source: String) -> String {
    let mut ast = ast::construct_ast(source);
    flatten_ast(&mut ast);
    return format!("{:#?}", ast);
}
