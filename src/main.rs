use crate::codegen::codegen::codegen;

use std::env;
use std::fs;
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
    let args: Vec<String> = env::args().collect();
    let src_file = args.get(1).expect("expects at least 1 arguments");
    let source = fs::read_to_string(src_file).expect("cannot read file");
    println!("{}", codegen(source.to_string()));
}
