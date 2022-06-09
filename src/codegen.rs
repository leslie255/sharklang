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

    // import needed externs from C standard library
    let mut code_externs = String::new();
    let mut externs: HashMap<&str, bool> = HashMap::new(); // avoid duplicates

    // data section of generated code
    let mut code_data_sect = String::from("\tsection\t.data\n");
    let mut literal_ids: HashMap<String, String> = HashMap::new();

    for node in &ast {
        match node {
            ast::Expression::FuncCall(name, args) => {
                // right now there's only one built-in function 'print'
                if name == "print" && !externs.contains_key("_printf") {
                    code_externs.push_str("\textern _printf\n");
                    externs.insert("_printf", true);
                }
                args.iter().for_each(|arg| {
                    data_sect_gen_literal(&mut literal_ids, arg, &mut code_data_sect)
                });
            }
            _ => {
                data_sect_gen_literal(&mut literal_ids, node, &mut code_data_sect);
            }
        }
    }

    // generate text section
    let mut code_text_sect = String::from("\tsection\t.text\n");
    let main_func_head = "\tglobal _main\n_main:\n\tpush\trbp\n";
    let main_func_tail = "\tpop\trbp\n\tmov\trax, 0\n\tret\n";
    code_text_sect.push_str(main_func_head);

    // this piece of shit is temporary, currently the only callable function is `print`, in the
    // future it will change
    for expr in &ast {
        match expr {
            ast::Expression::FuncCall(name, args) => match name.as_str() {
                "print" => {
                    let arg1 = args
                        .first()
                        .unwrap_or_else(|| panic!("no arguments supplied for `print`"));
                    match arg1 {
                        ast::Expression::StringLiteral(str) => {
                            code_text_sect.push_str(
                                format!(
                                    "\tmov\trdi, {}\n\tmov\trax,0\n\tcall\t_printf\n",
                                    literal_ids.get(str).unwrap()
                                )
                                .as_str(),
                            );
                        }
                        _ => {}
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

    code_text_sect.push_str(main_func_tail);

    format!("{}\n{}\n{}\n", code_externs, code_data_sect, code_text_sect)
}
