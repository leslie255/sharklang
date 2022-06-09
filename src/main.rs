use std::io;

mod codegen;

fn _input() -> String {
    let mut input_str = String::new();
    io::stdin()
        .read_line(&mut input_str)
        .expect("failed to read line");
    input_str
}

fn main() {
    let source = String::from("(print \"hello\")
(add 1 2)
(print \"world\")");

    println!("{}", codegen::codegen(source));
}
