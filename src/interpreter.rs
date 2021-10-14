use std::collections::HashMap;

use crate::parser::Expr;
use crate::parser::{Token, FuncTree};

use crate::parser::ParseState;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Str(String),
    List(Vec<Value>),
}

pub fn interpret(parsed: ParseState) -> Value {
    match parsed.func_names.get("output") {
        Some(FuncTree::Func(id)) => evaluate(parsed.func_map[id].1.clone()),
        _ => panic!("output function not found"),
    }
}


fn evaluate(expr: Expr) -> Value {
    match expr {
        Expr::Number(n) => Value::Number(n),
        Expr::Str(s) => Value::Str(s),
        Expr::Call { func, args } => todo!(),
        Expr::The(_) => todo!(),
    }
}