use std::io;

mod ast;

fn _input() -> String {
    let mut input_str = String::new();
    io::stdin()
        .read_line(&mut input_str)
        .expect("failed to read line");
    input_str
}

#[allow(unused)]
fn main() {
    let source = String::from("
(let a = 2)
(set a = 3)
(add 2 (minus 2 3))
(print \"hello world!\")");
    println!("source:{}", source);
    print!("\n");

    let ast = ast::construct_ast(source);
    println!("{:#?}", ast);
}
