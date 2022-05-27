use std::io;

mod ast;

fn _input() -> String {
    let mut input_str = String::new();
    io::stdin()
        .read_line(&mut input_str)
        .expect("failed to read line");
    input_str
}

fn main() {
    let source = "(add 1 (add 2 3))".to_string();
    println!("source:\n{}", source);
    print!("\n");

    let root_node = ast::construct_ast(source);
    println!("AST:");
    println!("{:?}", root_node);
}
