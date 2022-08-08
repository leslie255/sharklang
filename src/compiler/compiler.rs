use mir::ir::*;
use mir::fileformat::*;
use super::ast::*;
use super::builtin_funcs::BuiltinFuncChecker;
use super::checks::syntaxcheck::*;
use super::checks::typecheck::*;
use super::error::*;
use super::preprocess::*;
use super::tokens::*;
use super::typeinfer::*;

pub fn compile(source: String, src_file: String, file_format: FileFormat) -> String {
    let mut err_collector = ErrorCollector::new(src_file, &source);
    let tokens = preprocess(parse_tokens(&source));
    let mut ast = construct_ast(tokens, &mut err_collector);

    syntax_check(&mut err_collector, &ast);
    err_collector.print_errs();

    infer_type(&mut ast, &mut err_collector);
    err_collector.print_errs();

    let builtin_fns = BuiltinFuncChecker::new();

    print_ast!(ast.nodes);
    type_check(&ast, &builtin_fns, &mut err_collector);
    err_collector.print_errs();

    let program = Program::default();

    err_collector.print_errs();

    println!("{:#?}", program.content);

    let generated = mir::generation::x86_64::generate_asm(program, file_format);
    generated
}
