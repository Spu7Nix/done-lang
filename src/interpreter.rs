use internment::LocalIntern;

use crate::parser::{Expr, FuncInfo, NamedFunc, PatContent, PatternExpr};
use crate::parser::{FuncContent, WordTree};

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
    pub pat_map: &'a [(FuncInfo, PatContent)],
}

impl<'a> State<'a> {
    pub fn new(parsed: &'a ParseState) -> Self {
        Self {
            func_map: &parsed.func_map,
            pat_map: &parsed.pat_map,
        }
    }
}

pub fn interpret(parsed: ParseState) -> Value {
    let mut state = State::new(&parsed);
    //dbg!(&parsed.func_names);
    match parsed.names.branches().get(&LocalIntern::from("output")) {
        Some(WordTree::Leaf(NamedFunc {
            perfectum: Some(id),
            ..
        })) => evaluate_expr(
            match state.func_map[id.0].1.clone() {
                FuncContent::Custom(e) => e,
                FuncContent::Builtin(_) => unreachable!(),
                FuncContent::Uninitialized => unreachable!(),
            },
            &mut state,
            &[],
        ),
        _ => panic!("output function not found"),
    }
}

fn evaluate_expr(expr: Expr, state: &mut State, args: &[Value]) -> Value {
    match expr {
        Expr::Number(n) => Value::Number(n),
        Expr::Str(s) => Value::Str(s.to_string()),
        Expr::Call {
            func,
            args: call_args,
        } => {
            let evaled = call_args
                .into_iter()
                .map(|a| evaluate_expr(a, state, args))
                .collect::<Vec<_>>();

            match &state.func_map[func.0] {
                (_, FuncContent::Builtin(b)) => b(evaled),
                (_, FuncContent::Custom(e)) => evaluate_expr(e.clone(), state, &evaled),
                (_, FuncContent::Uninitialized) => unreachable!(),
            }
        }
        Expr::ArgRef(i) => args[i].clone(),
        Expr::If {
            condition,
            then,
            otherwise,
        } => {
            let evaled = evaluate_pattern_expr(condition, state, args);
            if evaled {
                evaluate_expr(*then, state, args)
            } else {
                evaluate_expr(*otherwise, state, args)
            }
        }
        Expr::List(l) => {
            let evaled = l
                .into_iter()
                .map(|a| evaluate_expr(a, state, args))
                .collect::<Vec<_>>();
            Value::List(evaled)
        }
    }
}

fn evaluate_pattern_expr(expr: PatternExpr, state: &mut State, args: &[Value]) -> bool {
    match expr {
        PatternExpr::Match {
            pat,
            args: call_args,
            not,
        } => {
            let evaled = call_args
                .into_iter()
                .map(|a| evaluate_expr(a, state, args))
                .collect::<Vec<_>>();
            let result = match &state.pat_map[pat.0].1 {
                PatContent::Custom(p) => evaluate_pattern_expr(p.clone(), state, &evaled),
                PatContent::Builtin(b) => b(evaled),
                PatContent::Uninitialized => unreachable!(),
            };
            if not {
                !result
            } else {
                result
            }
        }
        PatternExpr::And(_, _) => todo!(),
        PatternExpr::Or(_, _) => todo!(),
    }
}
