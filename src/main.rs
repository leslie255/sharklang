use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;

mod compiler;

fn compile(src_path: String, output_path: String, file_format: compiler::ir::FileFormat) {
    let source = fs::read_to_string(src_path.clone()).expect("cannot read file");
    let output = compiler::compiler::compile(source, src_path, file_format);

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
    let tokens = compiler::preprocess::preprocess(compiler::tokens::parse_tokens(&source));
    let mut err_collector = compiler::error::ErrorCollector::new(src_path, &source);
    let mut ast = compiler::ast::construct_ast(tokens, &mut err_collector);
    compiler::typeinfer::infer_type(&mut ast, &mut err_collector);
    print_ast!(ast.nodes);
    if !err_collector.errors.is_empty() {
        err_collector.print_errs();
    }
}

fn print_tokens(src_path: String) {
    let source = fs::read_to_string(src_path).expect("cannot read file");
    let tokens = compiler::tokens::parse_tokens(&source).tokens;
    for token in tokens {
        println!("{}\t{:?}", token.position, token.content);
    }
}

fn print_help(arg0: String) {
    println!("SHARK COMPILER v0.0.1");
    println!("\nTo compile a file:");
    println!("{} source_file.shark \\", arg0);
    println!("\t-o / --output: output_file.asm");
    println!("\t-f / --format: elf64 for GNU/Linux, macho64 for macOS");
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
    let mut file_format = compiler::ir::FileFormat::Elf64;

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
                    print_tree(src_path);
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
                        "elf64" => compiler::ir::FileFormat::Elf64,
                        "macho64" => compiler::ir::FileFormat::Macho64,
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

    compile(src_path, output_path, file_format);
}
