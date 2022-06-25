#[macro_use]
pub mod ir;
#[macro_use]
pub mod ast;
pub mod codegen;
pub mod tokens;
pub mod builtin_funcs;
pub mod preprocess;
pub mod typecheck;
pub mod error;

