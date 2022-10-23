use super::{
    tokens::{Token, TokenContent},
    typesystem::TypeExpr,
};
use std::{fmt::Display, rc::Rc};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ErrorContent {
    UnexpectedToken {
        expected: Vec<TokenContent>,
        found: TokenContent,
    },
    UnableToInferType {
        var_name: Rc<String>,
    },
    MismatchedType {
        expected: TypeExpr,
        found: Option<TypeExpr>,
    },
    SymbolNotExist(Rc<String>),
    NotAFunc(Rc<String>),
    NotAVar(Rc<String>),
    NotATypeName(Rc<String>),
    InvalidNumberFormat(String),
    IncorrectArgCount {
        expected: usize,
        found: usize,
    },
    IncorrectArgCountVariadic {
        expected: usize,
        found: usize,
    },
    Raw(String),
    MissingReturn(TypeExpr),
}
impl Display for ErrorContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { expected, found } => {
                if expected.is_empty() {
                    write!(f, "Unexpected token {}", found.name())?;
                } else {
                    write!(f, "Expects ")?;
                    let count = expected.len();
                    if count == 1 {
                        write!(f, "{}", expected[0].name())?;
                    } else {
                        for (i, token_content) in expected.iter().enumerate() {
                            if i == count - 1 {
                                // last one
                                write!(f, " or {}", token_content.name())?;
                            } else if i == count - 2 {
                                // second last one
                                write!(f, "{}", token_content.name())?;
                            } else {
                                write!(f, "{}, ", token_content.name())?;
                            }
                        }
                    }
                    write!(f, "; but found {}", found.name())?;
                }
            }
            Self::UnableToInferType { var_name } => {
                write!(
                    f,
                    "Enable to infer a type for variable `{}`, try manually specify a type",
                    var_name
                )?;
            }
            Self::MismatchedType { expected, found } => {
                write!(
                    f,
                    "Expects expression of type {expected}{}",
                    if let Some(t) = found {
                        format!(", found {t}")
                    } else {
                        "".to_string()
                    }
                )?;
            }
            Self::SymbolNotExist(name) => write!(f, "`{name}` is not a defined symbol")?,
            Self::NotAFunc(name) => {
                write!(f, "`{name}` is not a function")?;
            }
            Self::NotAVar(name) => write!(f, "`{name}` is not a variable")?,
            Self::NotATypeName(name) => write!(f, "`{name}` is not a type name")?,
            Self::InvalidNumberFormat(s) => write!(f, "`{s}` is not a valid number format")?,
            Self::IncorrectArgCount { expected, found } => {
                write!(f, "Expects {expected} arguments, found {found}")?
            }
            Self::IncorrectArgCountVariadic { expected, found } => {
                write!(f, "Expects at least {expected} arguments, found {found}")?
            }
            Self::MissingReturn(ret_type) => write!(f, "This function has a return value of type {ret_type}, but not all branches have a return statement")?,
            Self::Raw(message) => message.fmt(f)?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub content: ErrorContent,
    pub position: usize,
    pub length: usize, // use usize::MAX for just displaying one line
}
impl CompileError {
    pub fn unexpected_token_multiple(expected: Vec<TokenContent>, found: &Token) -> Self {
        Self {
            content: ErrorContent::UnexpectedToken {
                expected,
                found: found.content.clone(),
            },
            position: found.position,
            length: found.len,
        }
    }
    pub fn unexpected_token(expected: TokenContent, found: &Token) -> Self {
        Self {
            content: ErrorContent::UnexpectedToken {
                expected: vec![expected],
                found: found.content.clone(),
            },
            position: found.position,
            length: found.len,
        }
    }
    pub fn unexpected_token0(found: &Token) -> Self {
        Self {
            content: ErrorContent::UnexpectedToken {
                expected: Vec::new(),
                found: found.content.clone(),
            },
            position: found.position,
            length: found.len,
        }
    }
    pub fn not_a_func(id: &Rc<String>, pos: usize) -> Self {
        Self {
            content: ErrorContent::NotAFunc(Rc::clone(id)),
            position: pos,
            length: id.len(),
        }
    }
    pub fn not_a_var(id: &Rc<String>, pos: usize) -> Self {
        Self {
            content: ErrorContent::NotAVar(Rc::clone(id)),
            position: pos,
            length: id.len(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ErrorCollector {
    pub file_name: String,
    pub errors: Vec<CompileError>,
}

impl ErrorCollector {
    pub fn print_errs(&mut self, source: &String) {
        self.errors
            .sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap());

        for err in &self.errors {
            let mut line_str: String = String::new();
            let mut line: usize = 0;
            let mut column: usize = 0;
            let mut should_break_next_line = false;
            for (j, ch) in source.char_indices() {
                if !should_break_next_line {
                    column += 1;
                }
                if ch == '\n' {
                    line += 1;
                    if should_break_next_line {
                        break;
                    }
                    column = 0;
                    line_str = String::new();
                }
                line_str.push(ch);
                if j == err.position {
                    should_break_next_line = true;
                }
            }

            println!("{}:{}:{}\n{}", self.file_name, line, column, err.content);
            for ch in line_str.chars() {
                if ch == '\t' {
                    print!("    ");
                    column += 3;
                } else {
                    print!("{}", ch);
                }
            }
            if err.length != usize::MAX {
                println!();
                for _ in 0..column.saturating_sub(1) {
                    print!(" ");
                }
                print!("^");
                for _ in 1..err.length {
                    print!("~");
                }
            }
            println!("\n\n-------------------------------\n");
        }

        let err_count = self.errors.len();
        if err_count != 0 {
            println!(
                "failed to compile {} due to {}",
                self.file_name,
                if err_count == 1 {
                    "this error".to_string()
                } else {
                    format!("{} errors listed above", err_count)
                }
            );
            std::process::exit(101);
        }
    }
}

pub trait CollectError<T> {
    fn collect_error(self: Self, collector: &mut ErrorCollector) -> Option<T>;
}

impl<T> CollectError<T> for Result<T, CompileError> {
    fn collect_error(self: Self, collector: &mut ErrorCollector) -> Option<T> {
        match self {
            Ok(thing) => Some(thing),
            Err(e) => {
                collector.errors.push(e);
                None
            },
        }
    }
}
