use std::collections::HashMap;

use crate::parser::{Expr, FuncInfo};
use crate::parser::{FuncContent, FuncTree, Token};

use crate::parser::ParseState;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Str(String),
    List(Vec<Value>),
}

#[derive(Debug, Clone)]
pub struct State<'a> {
    pub func_map: &'a [(FuncInfo, FuncContent)],
}

impl<'a> State<'a> {
    pub fn new(parsed: &'a ParseState) -> Self {
        Self {
            func_map: &parsed.func_map,
        }
    }
}

pub fn interpret(parsed: ParseState) -> Value {
    let mut state = State::new(&parsed);
    match parsed.func_names.get("output") {
        Some(FuncTree::Func(id)) => evaluate(
            match state.func_map[id.0].1.clone() {
                FuncContent::Custom(e) => e,
                FuncContent::Builtin(_) => unreachable!(),
            },
            &mut state,
            &[],
        ),
        _ => panic!("output function not found"),
    }
}

fn evaluate(expr: Expr, state: &mut State, args: &[Value]) -> Value {
    match expr {
        Expr::Number(n) => Value::Number(n),
        Expr::Str(s) => Value::Str(s),
        Expr::Call {
            func,
            args: call_args,
        } => {
            let evaled = call_args
                .into_iter()
                .map(|a| evaluate(a, state, args))
                .collect::<Vec<_>>();

            match &state.func_map[func.0] {
                (_, FuncContent::Builtin(b)) => b(evaled),
                (_, FuncContent::Custom(e)) => evaluate(e.clone(), state, &evaled),
            }
        }
        Expr::ArgRef(i) => args[i].clone(),
    }
}
