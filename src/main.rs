pub(crate) use std::env;
use std::{fs, path::PathBuf};

use compiler::{
    ast::{parse_tokens_into_ast, AST},
    compiler::compile_shir_into_mir,
    error::ErrorCollector,
    preprocess::PreProcessor,
    shir::{ast_into_shir, SHIRProgram},
    tokens::*,
};
use mir::{fileformat::FileFormat, ir::Program};

mod compiler;
fn main() {
    let mut args = env::args();
    let arg0 = args.next().expect("what the fuck?");
    let mut src_path = String::new();
    let mut output_path = String::from("output.asm");
    let mut file_format = FileFormat::Elf64;

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
            "-m" | "--mir" => {
                if !src_path.is_empty() {
                    print_mir(src_path);
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
                        "elf64" => FileFormat::Elf64,
                        "macho64" => FileFormat::Macho64,
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

fn get_home_dir() -> Option<PathBuf> {
    Some(PathBuf::from(std::env::var("HOME").ok()?.trim()))
}

struct Compiler {
    error_collector: ErrorCollector,
    preprocessor: PreProcessor,
    content: Box<String>,
    tokens: Option<Vec<Token>>,
    ast: Option<AST>,
    shir: Option<SHIRProgram>,
    mir: Option<Program>,
}

impl Compiler {
    fn new(src_path: String) -> Compiler {
        let mut preprocessor = PreProcessor::new(src_path.clone(), Vec::new());
        if let Some(home_dir) = get_home_dir() {
            preprocessor
                .import_dirs
                .push(home_dir.join(".config/sharkc/import"))
        }
        Compiler {
            error_collector: ErrorCollector {
                file_name: src_path.clone(),
                errors: Vec::new(),
            },
            preprocessor,
            content: Box::new(fs::read_to_string(src_path.clone()).unwrap()),
            tokens: None,
            ast: None,
            shir: None,
            mir: None,
        }
    }
    fn generate_tokens(mut self) -> Compiler {
        if !self.error_collector.errors.is_empty() {
            return self;
        }
        self.tokens = Some(parse_into_tokens(&self.content, &mut self.preprocessor));
        self
    }
    fn generate_ast(mut self) -> Compiler {
        if !self.error_collector.errors.is_empty() {
            return self;
        }
        self.ast = Some(parse_tokens_into_ast(
            &mut TokenStream::from(&self.tokens.as_ref().unwrap()),
            &mut self.error_collector,
        ));
        self.tokens = None;
        self
    }
    fn generate_shir(mut self) -> Compiler {
        if !self.error_collector.errors.is_empty() {
            return self;
        }
        self.shir = Some(ast_into_shir(self.ast.unwrap(), &mut self.error_collector));
        self.ast = None;
        self
    }
    fn generate_mir(mut self) -> Compiler {
        if !self.error_collector.errors.is_empty() {
            return self;
        }
        self.mir = Some(compile_shir_into_mir(self.shir.unwrap()));
        self.shir = None;
        self
    }
    fn dump_errs(mut self) -> Compiler {
        if !self.error_collector.errors.is_empty() {
            self.error_collector.print_errs(&self.content);
            std::process::exit(1);
        } else {
            self
        }
    }
    fn finish(self, file_format: FileFormat) -> String {
        mir::generation::x86_64::generate_asm(self.mir.unwrap(), file_format)
    }
}

#[allow(unused_variables)]
fn print_help(src_path: String) {
    todo!()
}

fn compile(src_path: String, output_path: String, file_format: FileFormat) {
    let compiled_asm = Compiler::new(src_path)
        .generate_tokens()
        .generate_ast()
        .generate_shir()
        .generate_mir()
        .dump_errs()
        .finish(file_format);
    if fs::write(output_path, compiled_asm).is_err() {
        println!("unable to write to output file path");
        std::process::exit(1);
    }
}

fn print_mir(src_path: String) {
    let mir = Compiler::new(src_path)
        .generate_tokens()
        .generate_ast()
        .generate_shir()
        .generate_mir()
        .dump_errs()
        .mir
        .unwrap();
    for top_lvl in mir.content {
        println!("{top_lvl}")
    }
}

fn print_ir(src_path: String) {
    let ir = Compiler::new(src_path)
        .generate_tokens()
        .generate_ast()
        .generate_shir()
        .dump_errs()
        .shir
        .unwrap();
    println!("{:#?}", ir);
}

fn print_ast(src_path: String) {
    let ast = Compiler::new(src_path)
        .generate_tokens()
        .generate_ast()
        .dump_errs()
        .ast
        .unwrap();
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
    let tokens = Compiler::new(src_path).generate_tokens().tokens.unwrap();
    for token in &tokens {
        println!("{:?}\t{}\t{}", token.content, token.position, token.len);
    }
}
