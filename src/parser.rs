use core::panic;
use std::collections::HashMap;

use logos::Logos;

use crate::interpreter::Value;
use internment::LocalIntern;
pub type Symbol = LocalIntern<String>;

#[derive(Logos, Debug, PartialEq, Eq)]
enum LogosToken {
    //keywords
    #[token("the")]
    The,

    #[token("if")]
    If,

    #[token("is")]
    Is,

    #[token(".")]
    Period,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[regex(r"[0-9]+(\.[0-9]+)?", priority = 1)]
    Number,

    #[regex(r#"[a-z]?"(?:\\.|[^\\"])*"|'(?:\\.|[^\\'])*'"#)]
    StringLiteral,

    #[regex(r#"([A-Z]\w*)|number|string|list"#, priority = 1)]
    TypeName,

    #[regex(r#"[^A-Z\W]\w*"#, priority = 0)]
    Symbol,

    #[error]
    #[regex(r"[ \t\f\n\r]+|/\*[^*]*\*(([^/\*][^\*]*)?\*)*/|//[^\n]*", logos::skip)]
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    The(Symbol),
    If,
    Is,
    Period,
    Colon,
    Comma,
    Number(f64),
    StringLiteral(Symbol),
    Symbol(Symbol),
    TypeName(Symbol),
}

#[derive(Debug)]
pub struct Tokens {
    vec: Vec<Token>,
    index: usize,
}

impl Tokens {
    pub fn new(vec: &[Token]) -> Self {
        Self {
            vec: vec.to_vec(),
            index: 0,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.index < self.vec.len() {
            let out = self.vec[self.index].clone();
            self.index += 1;
            Some(out)
        } else {
            None
        }
    }

    pub fn previous(&mut self) -> Option<Token> {
        if self.index > 0 {
            self.index -= 1;
            let out = self.vec[self.index].clone();
            Some(out)
        } else {
            None
        }
    }
}

pub fn lex(text: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut iter = LogosToken::lexer(text);
    while let Some(token) = iter.next() {
        tokens.push(match token {
            LogosToken::The => Token::The({
                assert_eq!(iter.next().unwrap(), LogosToken::TypeName);
                Symbol::from(iter.slice())
            }),
            LogosToken::If => Token::If,
            LogosToken::Is => Token::Is,
            LogosToken::Period => Token::Period,
            LogosToken::Colon => Token::Colon,
            LogosToken::Comma => Token::Comma,
            LogosToken::Number => Token::Number(iter.slice().parse().unwrap()),
            LogosToken::StringLiteral => Token::StringLiteral({
                let slice = iter.slice();
                Symbol::from(&slice[1..(slice.len() - 1)])
            }),
            LogosToken::Symbol => Token::Symbol(Symbol::from(iter.slice())),
            LogosToken::Error => todo!(),
            LogosToken::TypeName => Token::TypeName(Symbol::from(iter.slice())),
        })
    }
    tokens
}

#[derive(PartialEq, Clone, Debug)]
pub enum FuncTree {
    Func(FnPtr),
    Branches(HashMap<Symbol, FuncTree>),
}
#[derive(Debug)]
pub struct ParseState {
    pub func_names: HashMap<Symbol, FuncTree>,
    type_count: u32,
    pub type_map: HashMap<Symbol, Ty>,
    pub func_map: Vec<(FuncInfo, FuncContent)>,
    pub scope_args: Vec<Ty>,
}

use crate::builtin::BUILTINS;

#[derive(Debug, Clone)]
pub enum FuncContent {
    Custom(Expr),
    Builtin(fn(Vec<Value>) -> Value),
}

impl ParseState {
    pub fn new() -> Self {
        let mut out = Self {
            func_names: HashMap::new(),
            type_count: 0,
            type_map: HashMap::new(),
            func_map: Vec::new(),
            scope_args: Vec::new(),
        };
        for (name, types, f) in BUILTINS {
            let signature = FuncSignature {
                info: FuncInfo {
                    args: types
                        .iter()
                        .map(|a| out.get_type(Symbol::from(*a)))
                        .collect(),
                },
                name: name.iter().map(|a| Symbol::from(*a)).collect(),
            };

            out.insert_func(signature, FuncContent::Builtin(*f))
        }
        out
    }

    pub fn get_type(&mut self, name: Symbol) -> Ty {
        *self.type_map.entry(name).or_insert({
            self.type_count += 1;
            Ty(self.type_count)
        })
    }

    pub fn set_scope_args(&mut self, func: &FuncSignature) {
        self.scope_args = func.info.args.clone();
    }

    fn insert_func(&mut self, signature: FuncSignature, f: FuncContent) {
        let mut current_branch = &mut self.func_names;
        for name in &signature.name[..signature.name.len() - 1] {
            current_branch = match current_branch
                .entry(*name)
                .or_insert_with(|| FuncTree::Branches(HashMap::new()))
            {
                FuncTree::Func(_) => panic!("Function already exists"),
                FuncTree::Branches(b) => b,
            };
        }

        self.func_map.push((signature.info, f));
        current_branch.insert(
            *signature.name.last().unwrap(),
            FuncTree::Func(FnPtr(self.func_map.len() - 1)),
        );
    }
}

#[derive(PartialEq, Clone, Copy, Hash, Eq, Debug)]
pub struct Ty(u32);

#[derive(PartialEq, Clone, Copy, Hash, Eq, Debug)]
pub struct FnPtr(pub usize);

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Number(f64),
    Str(Symbol),
    Call { func: FnPtr, args: Vec<Expr> },
    ArgRef(usize),
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct FuncInfo {
    args: Vec<Ty>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct FuncSignature {
    info: FuncInfo,
    name: Vec<Symbol>,
}

pub fn parse_mod(tokens: &[Token]) -> ParseState {
    let mut funcs = ParseState::new();
    let mut tokens = Tokens::new(tokens);
    while tokens.next().is_some() {
        tokens.previous();
        parse_sentence(&mut tokens, &mut funcs);
    }

    funcs
}

// fn split_at_first<'a>(tokens: &'a [Token], t: Token) -> (&'a [Token<'a>], &'a [Token<'a>]) {
//     let out = tokens.split_at(tokens.iter().position(|a| *a == t).unwrap());
//     (out.0, &out.1[1..])
// }

fn parse_sentence(tokens: &mut Tokens, state: &mut ParseState) {
    let first_type = tokens.next();

    match (first_type, tokens.next()) {
        (Some(Token::TypeName(t)), Some(Token::Is)) => {
            todo!();
            // pattern def
            //let (pattern, condition) = split_at_first(tokens, Token::If);
        }
        _ => {
            tokens.previous();
            tokens.previous();
            let mut def_part = Vec::new();
            loop {
                match tokens.next() {
                    Some(Token::Is) => break,
                    Some(a) => def_part.push(a),
                    None => panic!("expected type name, symbol or \"is\""),
                }
            }
            // func
            let signature = parse_func_def(&def_part, state);
            state.set_scope_args(&signature);
            let f = parse_value(tokens, state, ExprParseAmount::Exhaustive);
            state.insert_func(signature, FuncContent::Custom(f));
        }
    };

    if tokens.next() != Some(Token::Period) {
        panic!("Expected period")
    }
}

fn parse_func_def(s: &[Token], state: &mut ParseState) -> FuncSignature {
    let mut args = Vec::new();
    if let Token::TypeName(t) = &s[0] {
        args.push(state.get_type(*t));
    }
    let mut i = args.len(); // is start arg: 1, else 0
    let mut name = Vec::new();
    while let Some(Token::Symbol(s)) = s.get(i) {
        name.push(*s);
        i += 1;
    }

    if !args.is_empty() {
        if let Some(Token::TypeName(t)) = &s.get(i) {
            args.push(state.get_type(*t));
        }
    }

    FuncSignature {
        info: FuncInfo { args },
        name,
        //args: todo!(),
    }
}

#[derive(PartialEq, Eq)]
enum ExprParseAmount {
    Value,
    Exhaustive,
}

fn parse_value(tokens: &mut Tokens, state: &mut ParseState, amount: ExprParseAmount) -> Expr {
    let exhaustive = amount == ExprParseAmount::Exhaustive
        || if tokens.next() == Some(Token::Colon) {
            true
        } else {
            tokens.previous();
            false
        };
    let current_expr = match tokens.next() {
        Some(Token::The(a)) => {
            let typ = state.get_type(a);
            let mut list = state.scope_args.iter().enumerate().filter_map(|(i, a)| {
                if *a == typ {
                    Some(i)
                } else {
                    None
                }
            });
            let index = list.next().expect("No argument of this type");
            if list.next() != None {
                panic!("ambiguous arg ref")
            }
            Expr::ArgRef(index)
        }
        Some(Token::Number(n)) => (Expr::Number(n)),
        Some(Token::StringLiteral(s)) => (Expr::Str(s)),
        Some(Token::Symbol(_)) => parse_expression(None, tokens, state),
        Some(Token::TypeName(_)) => todo!(),
        a => panic!("unexpected {:?}", a),
    };
    if exhaustive {
        parse_expression(Some(current_expr), tokens, state)
    } else {
        current_expr
    }
}

fn parse_expression(
    mut current_expr: Option<Expr>,
    tokens: &mut Tokens,
    state: &mut ParseState,
) -> Expr {
    loop {
        current_expr = Some(match tokens.next() {
            Some(Token::Symbol(s)) => {
                let mut signature = vec![s];

                if let Some(mut b) = state.func_names.get(&s) {
                    loop {
                        match b {
                            &FuncTree::Func(ptr) => {
                                let mut args = current_expr.into_iter().collect::<Vec<_>>();

                                if state.func_map[ptr.0].0.args.len() == 2 {
                                    args.push(parse_value(tokens, state, ExprParseAmount::Value));
                                    //return Expr::Call { func: ptr, args };
                                }
                                break Expr::Call { func: ptr, args };
                            }
                            FuncTree::Branches(new_map) => match tokens.next() {
                                Some(Token::Symbol(s)) => {
                                    signature.push(s);
                                    match new_map.get(&s) {
                                        Some(b2) => b = b2,
                                        None => panic!("undefined function: {:?}", signature),
                                    }
                                }
                                a => panic!("expected symbol, found: {:?}", a),
                            },
                        }
                    }
                } else {
                    panic!("undefined function start: {}", s)
                }
            }
            Some(Token::Comma) => {
                if let Some(e) = current_expr {
                    parse_expression(Some(e), tokens, state)
                } else {
                    panic!("unexpected comma")
                }
            }
            Some(Token::Period) => {
                tokens.previous();
                return current_expr.unwrap();
            }
            a => panic!("unexpected {:?}", a),
        });
    }
}
