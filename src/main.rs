use std::fs;

use crate::parser::parse_mod;

mod builtin;
mod interpreter;
mod parser;

fn main() {
    let unparsed = fs::read_to_string("test.done").unwrap();
    let lexed = parser::lex(&unparsed);

    let parsed = parser::parse_mod(&lexed);

    let interpreted = interpreter::interpret(parsed);

    println!("output: {:?}", interpreted);
}
