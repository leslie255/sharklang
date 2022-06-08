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
    let source = String::from("
(print) \"hello world!\";
(add) 1 2;");
    println!("source:\n{}", source);
    print!("\n");

    let ast = ast::construct_ast(source);
    //println!("{:#?}", ast);
}
