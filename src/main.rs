use std::fs;

mod builtin;
mod interpreter;
mod parser;

fn main() {
    // get file name from argument
    let args: Vec<String> = std::env::args().collect();
    let filename = &args[1];

    let unparsed = fs::read_to_string(filename).unwrap();
    let lexed = parser::lex(&unparsed);

    let parsed = parser::parse_mod(&lexed);

    let interpreted = interpreter::interpret(parsed);

    println!("output: {}", interpreted);
}
