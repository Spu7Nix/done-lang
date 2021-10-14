use std::fs;

use crate::parser::parse_mod;

mod interpreter;
mod parser;

fn main() {
    let unparsed = fs::read_to_string("test.done").unwrap();
    let lexed = dbg!(parser::lex(&unparsed));

    let parsed = dbg!(parser::parse_mod(&lexed));

    //let interpreted = interpreter::interpret(parsed);
}
