include!("token.rs");

#[derive(Debug)]
pub enum NodeValue {
    FuncName(String),
    Number(isize),
}

#[derive(Debug)]
pub struct Node {
    pub value: NodeValue,
    pub sub_nodes: Vec<Node>,
}

fn recursive_parse_node(tokens: &Vec<Token>, i: &mut usize, node: &mut Node) {
    let mut current_token = &tokens[*i];
    macro_rules! increment {
        () => {
            *i += 1;
            if i < &mut tokens.len() {
                current_token = &tokens[*i];
            } else {
                break;
            }
        };
    }
    loop {
        if current_token.value == "(" {
            increment!();
            node.value = NodeValue::FuncName(current_token.value.clone());
            loop {
                increment!();
                if current_token.value == ")" {
                    break;
                } else if current_token.value == "(" {
                    node.sub_nodes.push(Node {
                        value: NodeValue::FuncName(String::new()), 
                        sub_nodes: Vec::new(),
                    });
                    recursive_parse_node(tokens, i, node.sub_nodes.last_mut().unwrap());
                } else {
                    node.sub_nodes.push(Node {
                        value: NodeValue::Number(current_token.value.trim().parse().unwrap()),
                        sub_nodes: Vec::new(),
                    });
                }
            }
        }
        increment!();
    }
}

pub fn construct_ast(source: String) -> Node {
    let mut root = Node {
        value: NodeValue::Number(0),
        sub_nodes: Vec::new(),
    };

    let tokens = parse_tokens(source);
    println!("tokens:");
    for token in &tokens {
        println!("{:?}: {}", token.class, token.value);
    }
    print!("\n");

    let mut i = 0;
    recursive_parse_node(&tokens, &mut i, &mut root);

    root
}
