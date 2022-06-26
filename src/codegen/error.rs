use super::tokens::*;

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum ErrorType {
    Syntax,
    Type,
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub err_type: ErrorType,
    pub message: String,
    pub position: usize,
    pub length: usize,
}

impl CompileError {
    pub fn new(
        err_type: ErrorType,
        message: String,
        position: usize,
        length: usize,
    ) -> CompileError {
        CompileError {
            err_type,
            message,
            position,
            length,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ErrorCollector<'a> {
    pub file_name: String,
    pub source: &'a String,
    pub errors: Vec<CompileError>,
}

impl<'a> ErrorCollector<'a> {
    pub fn new(file_name: String, source: &'a String) -> ErrorCollector<'a> {
        ErrorCollector {
            file_name,
            source: &source,
            errors: Vec::new(),
        }
    }
    #[allow(unused)]
    pub fn add_err(&mut self, err_type: ErrorType, position: usize, len: usize, message: String) {
        self.errors
            .push(CompileError::new(err_type, message, position, len));
    }
    pub fn syntax_err(&mut self, token: &Token, message: String) {
        self.errors.push(CompileError::new(
            ErrorType::Syntax,
            message,
            token.position,
            token.len,
        ));
    }
}

impl<'a> ErrorCollector<'a> {
    pub fn print_errs(&mut self) {
        self.errors
            .sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap());

        for err in &self.errors {
            let mut line_str: String = String::new();
            let mut line: usize = 0;
            let mut column: usize = 0;
            let mut should_break_next_line = false;
            for (j, ch) in self.source.char_indices() {
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

            println!(
                "{:?} error at {}:{}:{}\n{}",
                err.err_type,
                self.file_name,
                line + 1,
                column + 1,
                err.message,
            );
            println!("{}", line_str);
            for _ in 0..column - 1 {
                print!(" ");
            }
            print!("^");
            for _ in 1..err.length {
                print!("~");
            }
            println!("\n");
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
            std::process::exit(1);
        }
    }
}
