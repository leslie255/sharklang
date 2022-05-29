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
    println!("{}", '\n'.is_whitespace());
    let source = String::from(
        "
let a = 10;
var b = 20;
",
    );
    println!("source:\n{}", source);
    print!("\n");

    ast::construct_ast(source);
}
