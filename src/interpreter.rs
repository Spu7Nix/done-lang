use std::collections::HashMap;

use crate::parser::Token;

use crate::parser::ParseState;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Str(String),
    List(Vec<Value>),
}

pub fn interpret(tokens: ParseState) {}
