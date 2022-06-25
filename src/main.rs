use crate::codegen::codegen::codegen;

use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::path::Path;

mod codegen;

fn _input() -> String {
    let mut input_str = String::new();
    io::stdin()
        .read_line(&mut input_str)
        .expect("failed to read line");
    input_str
}

fn compile(src_path: String, output_path: String) {
    let source = fs::read_to_string(src_path.clone()).expect("cannot read file");
    let output = codegen(source, src_path);

    if Path::new(&output_path).exists() {
        fs::remove_file(output_path.clone()).unwrap_or_else(|_| {
            panic!(
                "Output file {} already exists and cannot be deleted",
                output_path
            )
        });
    }

    let mut output_file = fs::File::create(output_path.clone())
        .unwrap_or_else(|_| panic!("cannot write to file (0) {}", output_path));
    write!(output_file, "{}", output)
        .unwrap_or_else(|_| panic!("cannot write to file (1) {}", output_path));
}

fn print_tree(src_path: String) {
    let source = fs::read_to_string(src_path.clone()).expect("cannot read file");
    let tokens = codegen::tokens::parse_tokens(&source);
    let mut err_collector = codegen::error::ErrorCollector::new(src_path, &source);
    let ast = codegen::ast::construct_ast(tokens, &mut err_collector);
    if err_collector.errors.is_empty() {
        print_ast!(ast.nodes);
    } else {
        err_collector.print_errs();
    }
}

fn print_tokens(src_path: String) {
    let source = fs::read_to_string(src_path).expect("cannot read file");
    let tokens = codegen::tokens::parse_tokens(&source).tokens;
    for token in tokens {
        println!("{}\t{:?}", token.position, token.content);
    }
}

fn print_help(arg0: String) {
    println!("SHARK COMPILER v0.0.1");
    println!("\nTo compile a file:");
    println!("{} source_file.shark", arg0);
    println!("{} source_file.shark -o output.asm", arg0);
    println!("\nDebug:");
    println!("-t / --tokens: print tokens");
    println!("-a / --ast: print AST");
    println!("\n-h / --help: print this message");
}

fn main() {
    let mut args = env::args();
    let arg0 = args.next().expect("what the fuck?");
    let mut src_path = String::new();
    let mut output_path = String::from("output.asm");

    loop {
        let arg = match args.next() {
            Some(x) => x,
            None => break,
        };
        match arg.to_lowercase().as_str() {
            "-h" | "--help" => {
                print_help(arg0);
                return;
            }
            "-o" | "--output" => {
                output_path = match args.next() {
                    Some(x) => x,
                    None => {
                        print_help(arg0);
                        println!();
                        println!("expects an arguments after `-o` for output file");
                        panic!();
                    }
                }
            }
            "-t" | "--tokens" => {
                if !src_path.is_empty() {
                    print_tokens(src_path);
                    return;
                } else {
                    print_help(arg0);
                    println!();
                    println!("expects a source file");
                    panic!();
                }
            }
            "-a" | "--ast" => {
                if !src_path.is_empty() {
                    print_tree(src_path);
                    return;
                } else {
                    print_help(arg0);
                    println!();
                    println!("expects a source file");
                    panic!();
                }
            }
            _ => src_path = arg,
        }
    }

    if src_path.is_empty() {
        print_help(arg0);
        println!();
        println!("expects a source file");
        panic!();
    }

    compile(src_path, output_path);
}
