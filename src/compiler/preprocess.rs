#![allow(unused)]
use std::{
    collections::{HashMap, HashSet},
    fs,
    iter::Peekable,
    path::{Path, PathBuf},
    str::CharIndices,
};

use super::tokens::{parse_into_tokens, CharCustomFuncs, Token};

fn join_path(original: &Path, tail: &String) -> String {
    original.join(tail.clone()).to_str().unwrap().to_string()
}

#[derive(Debug, Clone)]
pub struct PreProcessor {
    pins: HashSet<String>,
    macros: HashMap<String, String>,
    src_path: String,
    parent_path: PathBuf,
}

impl PreProcessor {
    pub fn new(src_path: String) -> Self {
        let path = Path::new(&src_path).parent().unwrap().to_path_buf();
        Self {
            pins: HashSet::new(),
            macros: HashMap::new(),
            src_path,
            parent_path: path,
        }
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
                let included_content = fs::read_to_string(joined_path.clone()).unwrap();
                // TODO: CompileError if unable to read file
                parse_into_tokens(&included_content, self)
                    .iter()
                    .map(|t| {
                        let mut t = t.clone();
                        t.position = word_start;
                        t.len = len;
                        t
                    })
                    .collect()
            }
            "macro" => {
                // go to the next non-whitespace character
                while let Some((_, ch)) = chars.next_if(|(_, c)| c.is_whitespace()) {}
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
                    panic!("Cannot recognize preprocessor command `#{id}`\tmacro table: {:?}", self.macros)
                }
            }
        }
    }
}
