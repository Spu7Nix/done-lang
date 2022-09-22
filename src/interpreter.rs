use std::fmt::Display;

use internment::LocalIntern;

use crate::parser::{
    Call, Expr, FuncInfo, Match, NamedFunc, PatContent, PatternExpr, PropContent, Ty,
};
use crate::parser::{FuncContent, WordTree};

use crate::parser::ParseState;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Str(String),
    List(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::List(l) => {
                if l.is_empty() {
                    write!(f, "new list")
                } else {
                    let mut out = String::from("list of ");
                    for el in &l[..(l.len() - 1)] {
                        out += &format!("{}, ", el);
                    }
                    out += &format!("and {}", l.last().unwrap());
                    write!(f, "{}", out)
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct State<'a> {
    pub func_map: &'a [(FuncInfo, FuncContent)],
    pub pat_map: &'a [(FuncInfo, PatContent)],
    pub prop_map: &'a [(Ty, PropContent)],
}

impl<'a> State<'a> {
    pub fn new(parsed: &'a ParseState) -> Self {
        Self {
            func_map: &parsed.func_map,
            pat_map: &parsed.pat_map,
            prop_map: &parsed.prop_map,
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
        Expr::Call(Call {
            func,
            args: call_args,
        }) => {
            let evaled = call_args
                .into_iter()
                .map(|a| evaluate_expr(a, state, args))
                .collect::<Vec<_>>();
            run_func(state, func, evaled)
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
        Expr::ListMap {
            list,
            func: Call {
                func,
                args: call_args,
            },
        } => {
            let list = if let Value::List(l) = evaluate_expr(*list, state, args) {
                l
            } else {
                panic!("expected list")
            };
            let evaled = call_args
                .into_iter()
                .map(|a| evaluate_expr(a, state, args))
                .collect::<Vec<_>>();
            Value::List(
                list.into_iter()
                    .map(|item| {
                        let mut new_args = vec![item];
                        new_args.extend(evaled.clone());
                        run_func(state, func, new_args)
                    })
                    .collect(),
            )
        }
        Expr::ListFilter { list, predicate } => {
            let list = if let Value::List(l) = evaluate_expr(*list, state, args) {
                l
            } else {
                panic!("expected list")
            };

            Value::List(
                list.into_iter()
                    .filter(|item| {
                        let mut new_args = args.to_vec();
                        new_args.push(item.clone());

                        evaluate_pattern_expr(predicate.clone(), state, &new_args)
                    })
                    .collect(),
            )
        }
        Expr::Prop { arg, func } => {
            let evaled = evaluate_expr(*arg, state, args);
            match &state.prop_map[func.0] {
                (_, PropContent::Builtin(b)) => b(evaled),
                (_, PropContent::Custom(e)) => evaluate_expr(e.clone(), state, &vec![evaled]),
                (_, PropContent::Uninitialized) => unreachable!(),
            }
        }
    }
}

fn run_func(state: &mut State, func: crate::parser::FnPtr, args: Vec<Value>) -> Value {
    match &state.func_map[func.0] {
        (_, FuncContent::Builtin(b)) => b(args),
        (_, FuncContent::Custom(e)) => evaluate_expr(e.clone(), state, &args),
        (_, FuncContent::Uninitialized) => unreachable!(),
    }
}

fn evaluate_pattern_expr(expr: PatternExpr, state: &mut State, args: &[Value]) -> bool {
    match expr {
        PatternExpr::Match(Match {
            pat,
            args: call_args,
            not,
        }) => {
            let evaled = call_args
                .into_iter()
                .map(|a| evaluate_expr(a, state, args))
                .collect::<Vec<_>>();
            eval_match(pat, evaled, not, state)
        }
        PatternExpr::And(_, _) => todo!(),
        PatternExpr::Or(_, _) => todo!(),
    }
}

fn eval_match(
    pat: crate::parser::PatPtr,
    evaled: Vec<Value>,
    not: bool,
    state: &mut State,
) -> bool {
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
