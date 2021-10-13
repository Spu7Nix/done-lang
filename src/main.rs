use crate::parser::parse_mod;

mod interpreter;
mod parser;

fn main() {
    let lexed = dbg!(parser::lex(
        "number rounded up is 10."
    ));

    let parsed = dbg!(parser::parse_mod(&lexed));
}
