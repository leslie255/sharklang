use super::tokens::{parse_into_tokens, CharCustomFuncs, Token, TokenContent};
use std::{
    collections::{HashMap, HashSet},
    fs,
    iter::Peekable,
    path::{Path, PathBuf},
    rc::Rc,
    str::CharIndices,
};

fn join_path(original: &Path, tail: &String) -> PathBuf {
    original.join(tail.clone())
}

pub struct PreProcessor {
    pub pins: HashSet<String>,
    pub macros: HashMap<String, String>,
    pub parent_path: PathBuf,
    pub import_dirs: Vec<PathBuf>,
}

impl PreProcessor {
    pub fn new(src_path: String, import_dirs: Vec<String>) -> Self {
        let path = Path::new(&src_path).parent().unwrap().to_path_buf();
        let import_paths = import_dirs
            .into_iter()
            .filter_map(|s| {
                let path = Path::new(&s).to_path_buf();
                if path.is_dir() {
                    Some(path)
                } else {
                    None
                }
            })
            .collect();
        Self {
            pins: HashSet::new(),
            macros: HashMap::new(),
            parent_path: path,
            import_dirs: import_paths,
        }
    }

    fn find_import_path(&self, file_name: String) -> Option<PathBuf> {
        let file_path = Path::new(&file_name);
        for import_dir in &self.import_dirs {
            let joined_path = import_dir.join(file_path);
            if joined_path.is_file() {
                return Some(joined_path);
            }
        }
        None
    }

    fn expand_external_file(&mut self, file_path: PathBuf, pos: usize, len: usize) -> Vec<Token> {
        let file_content = fs::read_to_string(file_path).unwrap();
        parse_into_tokens(&file_content, self)
            .into_iter()
            .map(|mut t| {
                t.position = pos;
                t.len = len;
                t
            })
            .collect()
    }

    /// Called after encountering the `#` character,
    /// iterator should start on the character immediately after the `#` character,
    /// e.g. for `#macro`, iterator should be on `m`
    /// TODO: Return CompileError if needed
    pub fn expand_macro(
        &mut self,
        chars: &mut Peekable<CharIndices>,
        word_start: usize,
    ) -> Vec<Token> {
        if chars
            .peek()
            .expect("unexpected EOF after `#`")
            .1
            .is_whitespace()
        {
            panic!("unexpected whitespace after `#`");
        }
        // get macro keyword
        let mut keyword = String::with_capacity(7); // the longest keyword `include` has 7 chars
        while let Some((_, ch)) = chars.next_if(|(_, c)| c.is_alphanumeric_or_underscore()) {
            keyword.push(ch);
        }
        match keyword.as_str() {
            "include" => {
                // go to the next non-whitespace character
                let mut len = 8; // 8: the length of "#include"
                while chars.next_if(|(_, c)| c.is_whitespace()).is_some() {
                    len += 1;
                }
                // get file name
                let mut file_name = String::new();
                while let Some((_, ch)) = chars.next_if(|(_, c)| *c != '\n') {
                    file_name.push(ch);
                    len += 1;
                }
                let joined_path = join_path(&self.parent_path, &file_name);
                self.expand_external_file(joined_path, word_start, len)
            }
            "import" => {
                // go to the next non-whitespace character
                let mut len = 8; // 8: the length of "#include"
                while chars.next_if(|(_, c)| c.is_whitespace()).is_some() {
                    len += 1;
                }
                // get file name
                let mut file_name = String::new();
                while let Some((_, ch)) = chars.next_if(|(_, c)| *c != '\n') {
                    file_name.push(ch);
                    len += 1;
                }
                let file_path = self
                    .find_import_path(file_name)
                    .expect("imported path does not exist");
                self.expand_external_file(file_path, word_start, len)
            }
            "macro" => {
                // go to the next non-whitespace character
                while chars.next_if(|(_, c)| c.is_whitespace()).is_some() {}
                // get macro name
                let mut name = String::new();
                while let Some((_, ch)) = chars.next_if(|(_, c)| !c.is_whitespace()) {
                    name.push(ch);
                }
                // go to the next non-whitespace character
                // if encountered one or more newline characters, it means it's a multi-line macro
                let mut is_multiline = false;
                while let Some((_, ch)) = chars.next_if(|(_, c)| c.is_whitespace()) {
                    if ch == '\n' {
                        is_multiline = true;
                    }
                }
                let mut macro_body = String::new();
                if !is_multiline {
                    // get all character until newline
                    while let Some((_, ch)) = chars.next_if(|(_, c)| *c != '\n') {
                        macro_body.push(ch);
                    }
                } else {
                    loop {
                        let ch = chars.next().expect("Unexpected EOF in multi-line macro").1;
                        if ch == '#' {
                            // if it's #end, end macro
                            let mut keyword = String::with_capacity(3);
                            for _ in 0..3 {
                                keyword.push(
                                    chars.next().expect("Unexpected EOF in multi-line macro").1,
                                );
                            }
                            if keyword == "end" {
                                break;
                            } else {
                                macro_body.push('#');
                                macro_body.push_str(keyword.as_str());
                                continue;
                            }
                        }
                        macro_body.push(ch);
                    }
                }
                self.macros.insert(name, macro_body);
                Vec::new()
            }
            "asm" => {
                let mut len = 4;
                let mut is_multiline = false;
                // go to the next non-whitespace character
                while let Some((_, ch)) = chars.next_if(|(_, c)| c.is_whitespace()) {
                    if ch == '\n' {
                        is_multiline = true;
                    }
                    len += 1;
                }
                let mut asm_code = String::new();
                if !is_multiline {
                    while let Some((_, ch)) = chars.next_if(|(_, c)| *c != '\n') {
                        asm_code.push(ch);
                        len += 1;
                    }
                } else {
                    loop {
                        let ch = chars.next().expect("Unexpected EOF in multi-line #asm").1;
                        if ch == '#' {
                            // if it's #end, end macro
                            let mut keyword = String::with_capacity(3);
                            for _ in 0..3 {
                                keyword.push(
                                    chars.next().expect("Unexpected EOF in multi-line #asm").1,
                                );
                            }
                            if keyword == "end" {
                                break;
                            } else {
                                asm_code.push('#');
                                asm_code.push_str(keyword.as_str());
                                continue;
                            }
                        }
                        asm_code.push(ch);
                    }
                }
                vec![Token {
                    content: TokenContent::RawASM(Rc::new(asm_code.trim().to_string())),
                    position: word_start,
                    len,
                }]
            }
            "pin" => {
                // go to the next non-whitespace character
                while chars.next_if(|(_, c)| c.is_whitespace()).is_some() {}
                // get pin name
                let mut name = String::new();
                while let Some((_, ch)) = chars.next_if(|(_, c)| !c.is_whitespace()) {
                    name.push(ch);
                }
                println!("Added pin `{name}`");
                self.pins.insert(name);
                Vec::new()
            }
            "ifpin" => todo!(),
            "elifpin" => todo!(),
            "else" => todo!(),
            "end" => panic!("unexpected macro command `#end`"),
            id => {
                let len = id.len() + 1; // +1 because of `#`
                if let Some(expanded_content) = self.macros.get(id) {
                    let expanded_content = expanded_content.clone();
                    parse_into_tokens(&expanded_content, self)
                        .iter()
                        .map(|t| {
                            let mut t = t.clone();
                            t.position = word_start;
                            t.len = len;
                            t
                        })
                        .collect()
                } else {
                    panic!(
                        "Cannot recognize preprocessor command `#{id}`\tmacro table: {:?}",
                        self.macros
                    )
                }
            }
        }
    }
}
