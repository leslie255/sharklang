use super::super::ast::*;
use super::super::error::*;

#[derive(Debug, Clone)]
struct AllowedExprs {
    allow_id: bool,
    allow_num: bool,
    allow_str: bool,
    allow_ch: bool,
    allow_fn_call: bool,
    allow_var_init: bool,
    allow_var_assign: bool,
    allow_type_cast: bool,
    allow_addr: bool,
    allow_deref: bool,
    allow_asm: bool,
    allow_block: bool,
    allow_fn_def: bool,
    allow_loop: bool,
    allow_if: bool,
    allow_ret: bool,
}

impl Default for AllowedExprs {
    fn default() -> Self {
        Self {
            allow_id: false,
            allow_num: false,
            allow_str: false,
            allow_ch: false,
            allow_fn_call: false,
            allow_var_init: false,
            allow_var_assign: false,
            allow_type_cast: false,
            allow_addr: false,
            allow_deref: false,
            allow_asm: false,
            allow_block: false,
            allow_fn_def: false,
            allow_loop: false,
            allow_if: false,
            allow_ret: false,
        }
    }
}
impl AllowedExprs {
    fn allow_id(mut self) -> Self {
        self.allow_id = true;
        self
    }
    fn allow_num(mut self) -> Self {
        self.allow_num = true;
        self
    }
    fn allow_str(mut self) -> Self {
        self.allow_str = true;
        self
    }
    fn allow_ch(mut self) -> Self {
        self.allow_ch = true;
        self
    }
    fn allow_fn_call(mut self) -> Self {
        self.allow_fn_call = true;
        self
    }
    fn allow_var_init(mut self) -> Self {
        self.allow_var_init = true;
        self
    }
    fn allow_var_assign(mut self) -> Self {
        self.allow_var_assign = true;
        self
    }
    fn allow_type_cast(mut self) -> Self {
        self.allow_type_cast = true;
        self
    }
    fn allow_addr(mut self) -> Self {
        self.allow_addr = true;
        self
    }
    fn allow_deref(mut self) -> Self {
        self.allow_deref = true;
        self
    }
    fn allow_asm(mut self) -> Self {
        self.allow_asm = true;
        self
    }
    fn allow_block(mut self) -> Self {
        self.allow_block = true;
        self
    }
    fn allow_fn_def(mut self) -> Self {
        self.allow_fn_def = true;
        self
    }
    fn allow_loop(mut self) -> Self {
        self.allow_loop = true;
        self
    }
    fn allow_if(mut self) -> Self {
        self.allow_if = true;
        self
    }
    fn allow_ret(mut self) -> Self {
        self.allow_ret = true;
        self
    }

    fn matches(&self, expr: &Expression) -> bool {
        match expr {
            Expression::Identifier(_) => self.allow_id,
            Expression::NumberLiteral(_) => self.allow_num,
            Expression::StringLiteral(_) => self.allow_str,
            Expression::CharLiteral(_) => self.allow_ch,
            Expression::FuncCall(_, _) => self.allow_fn_call,
            Expression::VarInit(_, _, _) => self.allow_var_init,
            Expression::VarAssign(_, _) => self.allow_var_assign,
            Expression::TypeCast(_, _) => self.allow_type_cast,
            Expression::GetAddress(_) => self.allow_addr,
            Expression::Dereference(_) => self.allow_deref,
            Expression::Label(_) => self.allow_asm,
            Expression::RawASM(_) => self.allow_asm,
            Expression::Block(_) => self.allow_block,
            Expression::FuncDef(_, _) => self.allow_fn_def,
            Expression::Loop(_) => self.allow_loop,
            Expression::If(_, _, _, _) => self.allow_if,
            Expression::ReturnVoid => self.allow_ret,
            Expression::ReturnVal(_) => self.allow_ret,
            Expression::UnsafeReturn => self.allow_ret,
            Expression::Unknown => false,
        }
    }
}

#[derive(Debug, Clone)]
struct SyntaxChecker {
    top_level_allowed: AllowedExprs,
    fn_body_allowed: AllowedExprs,
    loop_body_allowed: AllowedExprs,
    if_body_allowed: AllowedExprs,
    if_condition_allowed: AllowedExprs,
    fn_call_arg_allowed: AllowedExprs,
    rhs_allowed: AllowedExprs,
    deref_allowed: AllowedExprs,
    get_addr_allowed: AllowedExprs,
    type_cast_allowed: AllowedExprs,
}

impl Default for SyntaxChecker {
    fn default() -> Self {
        SyntaxChecker {
            top_level_allowed: AllowedExprs::default().allow_fn_def().allow_asm(),
            fn_body_allowed: AllowedExprs::default()
                .allow_str()
                .allow_fn_call()
                .allow_var_init()
                .allow_var_assign()
                .allow_asm()
                .allow_loop()
                .allow_if()
                .allow_ret(),
            loop_body_allowed: AllowedExprs::default()
                .allow_str()
                .allow_fn_call()
                .allow_var_assign()
                .allow_asm()
                .allow_loop()
                .allow_if()
                .allow_ret(),
            if_body_allowed: AllowedExprs::default()
                .allow_str()
                .allow_fn_call()
                .allow_var_assign()
                .allow_asm()
                .allow_loop()
                .allow_if()
                .allow_ret(),
            if_condition_allowed: AllowedExprs::default()
                .allow_id()
                .allow_num()
                .allow_ch()
                .allow_fn_call()
                .allow_deref(),
            fn_call_arg_allowed: AllowedExprs::default()
                .allow_id()
                .allow_num()
                .allow_str()
                .allow_ch()
                .allow_fn_call()
                .allow_type_cast()
                .allow_addr()
                .allow_deref()
                .allow_block(),
            rhs_allowed: AllowedExprs::default()
                .allow_id()
                .allow_num()
                .allow_str()
                .allow_ch()
                .allow_fn_call()
                .allow_type_cast()
                .allow_addr()
                .allow_deref(),
            deref_allowed: AllowedExprs::default()
                .allow_id()
                .allow_num()
                .allow_str()
                .allow_ch()
                .allow_fn_call(),
            get_addr_allowed: AllowedExprs::default()
                .allow_id()
                .allow_num()
                .allow_str()
                .allow_ch(),
            type_cast_allowed: AllowedExprs::default()
                .allow_id()
                .allow_num()
                .allow_str()
                .allow_ch()
                .allow_fn_call()
                .allow_deref()
                .allow_addr(),
        }
    }
}

fn recursive_check_inside_fn(
    err_collector: &mut ErrorCollector,
    ast: &AST,
    node: &ASTNode,
    syntax_checker: &SyntaxChecker,
    allowed_exprs: &AllowedExprs,
) {
    macro_rules! its_not_allowed_here {
        ($node: expr) => {
            err_collector.add_err(
                ErrorType::Syntax,
                $node.position,
                usize::MAX,
                format!("{} is not allowed here", $node.expr.description()),
            )
        };
    }
    if !allowed_exprs.matches(&node.expr) {
        its_not_allowed_here!(node);
        return;
    }
    match &node.expr {
        Expression::If(if_condition_i, if_block_i, elif_blocks_i, else_block_i) => {
            if !elif_blocks_i.is_empty() {
                todo!();
            }
            recursive_check_inside_fn(
                err_collector,
                ast,
                ast.node(*if_condition_i),
                syntax_checker,
                &syntax_checker.if_condition_allowed,
            );
            for if_body_node in ast
                .expr(*if_block_i)
                .get_block_unchecked()
                .body
                .iter()
                .map(|i| ast.node(*i))
            {
                recursive_check_inside_fn(
                    err_collector,
                    ast,
                    if_body_node,
                    syntax_checker,
                    &syntax_checker.if_body_allowed,
                );
            }
            if let Some(else_block) = ast.expr(*else_block_i).get_block() {
                for else_body_node in else_block.body.iter().map(|i| ast.node(*i)) {
                    recursive_check_inside_fn(
                        err_collector,
                        ast,
                        else_body_node,
                        syntax_checker,
                        &syntax_checker.if_body_allowed,
                    );
                }
            }
        }
        Expression::Loop(loop_block_i) => {
            for loop_body_node in ast
                .expr(*loop_block_i)
                .get_block_unchecked()
                .body
                .iter()
                .map(|i| ast.node(*i))
            {
                recursive_check_inside_fn(
                    err_collector,
                    ast,
                    loop_body_node,
                    syntax_checker,
                    &syntax_checker.loop_body_allowed,
                );
            }
        }
        Expression::FuncCall(_, args_i) => {
            for arg_node in args_i.iter().map(|i| ast.node(*i)) {
                recursive_check_inside_fn(
                    err_collector,
                    ast,
                    arg_node,
                    syntax_checker,
                    &syntax_checker.fn_call_arg_allowed,
                );
            }
        }
        Expression::VarInit(_, _, rhs_i) | Expression::VarAssign(_, rhs_i) => {
            recursive_check_inside_fn(
                err_collector,
                ast,
                ast.node(*rhs_i),
                syntax_checker,
                &syntax_checker.rhs_allowed,
            );
        }
        Expression::TypeCast(node_i, _) => {
            recursive_check_inside_fn(
                err_collector,
                ast,
                ast.node(*node_i),
                syntax_checker,
                &syntax_checker.type_cast_allowed,
            );
        }
        Expression::GetAddress(node_i) => {
            recursive_check_inside_fn(
                err_collector,
                ast,
                ast.node(*node_i),
                syntax_checker,
                &syntax_checker.get_addr_allowed,
            );
        }
        Expression::Dereference(node_i) => {
            recursive_check_inside_fn(
                err_collector,
                ast,
                ast.node(*node_i),
                syntax_checker,
                &syntax_checker.deref_allowed,
            );
        }
        Expression::Unknown => {
            its_not_allowed_here!(node);
        }
        _ => (),
    }
}

pub fn syntax_check(err_collector: &mut ErrorCollector, ast: &AST) {
    let syntax_checker = SyntaxChecker::default();
    for node in ast.nodes.iter().filter(|n| n.is_top_level) {
        if !syntax_checker.top_level_allowed.matches(&node.expr) {
            err_collector.add_err(
                ErrorType::Syntax,
                node.position,
                usize::MAX,
                format!("{} is not allowed at top level", node.expr.description()),
            );
            continue;
        }
        let fn_body = &if let Expression::FuncDef(_, block_i) = node.expr {
            ast.expr(block_i)
        } else {
            panic!();
        }
        .get_block_unchecked()
        .body;

        for node in fn_body.iter().map(|i| ast.node(*i)) {
            recursive_check_inside_fn(
                err_collector,
                ast,
                node,
                &syntax_checker,
                &syntax_checker.fn_body_allowed,
            );
        }
    }
}
