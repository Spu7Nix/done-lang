mod interpreter;
mod parser;

fn main() {
    dbg!(parser::lex(
        "number is whole if the number is equal to the number rounded."
    ));
}
