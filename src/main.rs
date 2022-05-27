use std::io;

mod tokens;

fn _input() -> String {
    let mut input_str = String::new();
    io::stdin()
        .read_line(&mut input_str)
        .expect("failed to read line");
    input_str
}

fn main() {
    let source = "(hello123 (add 1 2))\n".to_string();
    println!("source:\n{}", source);

    let tokens = tokens::parse_tokens(source);

    for token in tokens {
        println!("{:?}: {}", token.class, token.value);
    }
}
