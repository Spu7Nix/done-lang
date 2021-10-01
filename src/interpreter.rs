use std::collections::HashMap;

use crate::parser::Token;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Str(String),
    List(Vec<Value>),
}

#[derive(Debug, Clone)]
struct Tokens<'a> {
    data: &'a [Token<'a>],
    index: usize,
}

impl<'a> Tokens<'a> {
    fn next(&mut self) -> Token {
        let out = self.data[self.index];
        self.index += 1;
        out
    }

    fn new(data: &'a [Token]) -> Self {
        Tokens { data, index: 0 }
    }
}

#[derive(Debug, Clone, Copy, Hash)]
struct TypeId(usize);

struct State {
    types: HashMap<String, TypeId>,
}

impl State {
    fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }
}

pub fn interpret(tokens: Vec<Token>) {
    let statements = tokens.split(|a| *a == Token::Period);
    let mut state = State::new();

    for data in statements {
        let mut tokens = Tokens::new(data);

        let main_typ = match tokens.next() {
            Token::Symbol(s) => state.types[s],
            _ => panic!("Expected symbol"),
        };

        match tokens.next() {
            Token::Is => pattern_def(main_typ, &mut tokens),
            Token::Symbol(name) => {
                let cloned = name.to_string();
                declaration(main_typ, &cloned, &mut tokens)
            }
            _ => panic!("Expected symbol or `is`"),
        }
    }
}

fn declaration(main_typ: TypeId, name: &str, tokens: &mut Tokens) {
    todo!()
}

fn pattern_def(main_typ: TypeId, tokens: &mut Tokens) {
    todo!()
}
