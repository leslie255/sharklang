#[allow(unused_assignments)]
pub fn preprocess(source: String) -> String {
    let mut iter = source.chars();
    let mut processed = String::new();
    let mut last_ch = '\0';
    let mut ch = '\0';

    macro_rules! next {
        () => {{
            last_ch = ch;
            ch = match iter.next() {
                Some(x) => x,
                None => break,
            }
        }};
    }

    loop {
        next!();
        
        if ch == '/' && last_ch == '/' {
            processed.pop();
            while ch != '\n' {
                next!();
            }
        }
        processed.push(ch);
    }

    processed
}
