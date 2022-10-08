#![allow(unused)]

use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::rc::Weak;

use compiler::compiler::ast_into_shir;
use compiler::tokens::*;

use crate::compiler::ast::*;

mod compiler;
fn main() {
    let mut args = env::args();
    let arg0 = args.next().expect("what the fuck?");
    let mut src_path = String::new();
    let mut output_path = String::from("output.asm");
    let mut file_format = mir::fileformat::FileFormat::Elf64;

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
                        std::process::exit(1);
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
                    print_ast(src_path);
                    return;
                } else {
                    print_help(arg0);
                    println!();
                    println!("expects a source file");
                    panic!();
                }
            }
            "-i" | "--ir" => {
                if !src_path.is_empty() {
                    print_ir(src_path);
                    return;
                } else {
                    print_help(arg0);
                    println!();
                    println!("expects a source file");
                    panic!();
                }
            }
            "-f" | "--format" => {
                file_format = match args.next() {
                    Some(f) => match f.to_lowercase().as_str() {
                        "elf64" => mir::fileformat::FileFormat::Elf64,
                        "macho64" => mir::fileformat::FileFormat::Macho64,
                        _ => {
                            print_help(arg0);
                            println!();
                            println!("expect `macho64` or `elf64` after `-f` for file format");
                            std::process::exit(1);
                        }
                    },
                    None => {
                        print_help(arg0);
                        println!();
                        println!("expect `macho64` or `elf64` after `-f` for file format");
                        std::process::exit(1);
                    }
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

    //compile(src_path, output_path, file_format);
}

fn print_ir(src_path: String) {
    let source = fs::read_to_string(src_path).unwrap();
    let tokens = parse_into_tokens(&source);
    let mut token_stream = TokenStream::from(&tokens);

    let ast = parse_tokens_into_ast(&mut token_stream);
    let ir = ast_into_shir(ast);
    println!("{:#?}", ir);
}

fn print_help(src_path: String) {
    todo!()
}

fn print_ast(src_path: String) {
    let source = fs::read_to_string(src_path).unwrap();
    let tokens = parse_into_tokens(&source);
    let mut token_stream = TokenStream::from(&tokens);

    let ast = parse_tokens_into_ast(&mut token_stream);

    println!("String literals:");
    for (i, str) in ast.strliteral_pool.iter().enumerate() {
        println!("{}\t{:?}", i, str);
    }
    println!("-------------");
    for root_node in ast.root_nodes.iter().filter_map(|n| n.upgrade()) {
        println!("{:#?}\t{}", root_node.expr, root_node.pos);
    }
}

fn print_tokens(src_path: String) {
    let source = fs::read_to_string(src_path).unwrap();
    let tokens = parse_into_tokens(&source);
    for token in &tokens {
        println!("{:?}\t{}\t{}", token.content, token.position, token.len);
    }
}
