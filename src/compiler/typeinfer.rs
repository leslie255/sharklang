#![allow(unused)]
use super::ast::*;
use super::checks::typecheck::DataType;
use super::error::*;

pub fn infer_type(ast: &mut AST, err_collector: &mut ErrorCollector) {
    let mut ast_changes: Vec<(usize, ASTNode)> = Vec::new();
    for (block_i, fn_block) in ast.nodes.iter().filter_map(|n| {
        if let Expression::FuncDef(_, block_i) = n.expr {
            Some((block_i, ast.expr(block_i).get_block()?))
        } else {
            None
        }
    }) {
        let mut var_type_changes: Vec<(String, DataType)> = Vec::new();
        for (i, var_name, old_node, rhs_expr) in fn_block.body.iter().filter_map(|i| {
            if let Expression::VarInit(var_name, t, rhs_i) = ast.expr(*i) {
                if *t == DataType::ToBeDetermined {
                    Some((i, var_name, ast.node(*i), ast.expr(*rhs_i)))
                } else {
                    None
                }
            } else {
                None
            }
        }) {
            let infered_type = DataType::infer_from_expr(rhs_expr, fn_block, ast);
            if infered_type.is_none() {
                err_collector.add_err(
                    ErrorType::Type,
                    old_node.position,
                    usize::MAX,
                    format!("Unable to infer type from this expression"),
                );
                break;
            }
            var_type_changes.push((var_name.clone(), infered_type.clone().unwrap()));
            let mut new_var_init_node = old_node.clone();
            if let Expression::VarInit(_, t, _) = &mut new_var_init_node.expr {
                *t = infered_type.unwrap();
            }
            ast_changes.push((*i, new_var_init_node));
        }
        if !var_type_changes.is_empty() {
            let mut new_block = fn_block.clone();
            let mut new_block_node = ast.node(block_i).clone();
            if let Expression::Block(b) = &mut new_block_node.expr {
                *b = new_block;
            }
            ast_changes.push((block_i, new_block_node));
        }
    }
    for (i, new_node) in ast_changes {
        ast.nodes[i] = new_node;
    }
}
