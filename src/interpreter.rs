use std::collections::HashMap;

use crate::parser::Expr;
use crate::parser::{FuncContent, FuncTree, Token};

use crate::parser::ParseState;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Str(String),
    List(Vec<Value>),
}

pub fn interpret(parsed: ParseState) -> Value {
    match parsed.func_names.get("output") {
        Some(FuncTree::Func(id)) => evaluate(
            match parsed.func_map[id.0].1.clone() {
                FuncContent::Custom(e) => e,
                FuncContent::Builtin(_) => unreachable!(),
            },
            &parsed,
        ),
        _ => panic!("output function not found"),
    }
}

fn evaluate(expr: Expr, parsed: &ParseState) -> Value {
    match expr {
        Expr::Number(n) => Value::Number(n),
        Expr::Str(s) => Value::Str(s),
        Expr::Call { func, args } => {
            let evaled = args
                .into_iter()
                .map(|a| evaluate(a, parsed))
                .collect::<Vec<_>>();

            match &parsed.func_map[func.0] {
                (_, FuncContent::Builtin(b)) => b(evaled),
                (_, FuncContent::Custom(e)) => evaluate(e.clone(), parsed),
            }
        }
        Expr::The(_) => todo!(),
    }
}
