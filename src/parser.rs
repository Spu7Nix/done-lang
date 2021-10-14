use std::collections::HashMap;

use logos::Logos;

use crate::interpreter::Value;

pub type SymbolType = String;

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

    #[token(":=")]
    Define,

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
pub enum Token<'a> {
    The(&'a str),
    If,
    Is,
    Period,
    Define,
    Comma,
    Number(f64),
    StringLiteral(&'a str),
    Symbol(&'a str),
    TypeName(&'a str),
}

pub fn lex(text: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut iter = LogosToken::lexer(text);
    while let Some(token) = iter.next() {
        tokens.push(match token {
            LogosToken::The => Token::The({
                assert_eq!(iter.next().unwrap(), LogosToken::TypeName);
                iter.slice()
            }),
            LogosToken::If => Token::If,
            LogosToken::Is => Token::Is,
            LogosToken::Period => Token::Period,
            LogosToken::Define => Token::Define,
            LogosToken::Comma => Token::Comma,
            LogosToken::Number => Token::Number(iter.slice().parse().unwrap()),
            LogosToken::StringLiteral => Token::StringLiteral({
                let slice = iter.slice();
                &slice[1..(slice.len() - 1)]
            }),
            LogosToken::Symbol => Token::Symbol(iter.slice()),
            LogosToken::Error => todo!(),
            LogosToken::TypeName => Token::TypeName(iter.slice()),
        })
    }
    tokens
}

#[derive(PartialEq, Clone, Debug)]
pub enum FuncTree {
    Func(FnPtr),
    Branches(HashMap<SymbolType, FuncTree>),
}
#[derive(Debug)]
pub struct ParseState {
    pub func_names: HashMap<SymbolType, FuncTree>,
    type_count: u32,
    pub type_map: HashMap<SymbolType, Ty>,
    pub func_map: Vec<(FuncInfo, FuncContent)>,
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
        };
        for (name, types, f) in BUILTINS {
            let signature = FuncSignature {
                info: FuncInfo {
                    args: types
                        .iter()
                        .map(|a| out.get_type(SymbolType::from(*a)))
                        .collect(),
                },
                name: name.iter().map(|a| SymbolType::from(*a)).collect(),
            };

            out.insert_func(signature, FuncContent::Builtin(*f))
        }
        out
    }

    pub fn get_type(&mut self, name: SymbolType) -> Ty {
        *self.type_map.entry(name).or_insert({
            self.type_count += 1;
            Ty(self.type_count)
        })
    }

    fn insert_func(&mut self, signature: FuncSignature, f: FuncContent) {
        let mut current_branch = &mut self.func_names;
        for name in &signature.name[..signature.name.len() - 1] {
            current_branch = match current_branch
                .entry(name.clone())
                .or_insert_with(|| FuncTree::Branches(HashMap::new()))
            {
                FuncTree::Func(_) => panic!("Function already exists"),
                FuncTree::Branches(b) => b,
            };
        }

        self.func_map.push((signature.info, f));
        current_branch.insert(
            signature.name.last().unwrap().clone(),
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
    Str(SymbolType),
    Call { func: FnPtr, args: Vec<Expr> },
    The(Ty),
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct FuncInfo {
    args: Vec<Ty>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct FuncSignature {
    info: FuncInfo,
    name: Vec<SymbolType>,
}

pub fn parse_mod(tokens: &[Token]) -> ParseState {
    let mut funcs = ParseState::new();
    let sentences = tokens.split(|a| *a == Token::Period);
    for s in sentences {
        if !s.is_empty() {
            parse_sentence(s, &mut funcs);
        }
    }
    funcs
}

fn split_at_first<'a>(tokens: &'a [Token], t: Token) -> (&'a [Token<'a>], &'a [Token<'a>]) {
    let out = tokens.split_at(tokens.iter().position(|a| *a == t).unwrap());
    (out.0, &out.1[1..])
}

fn parse_sentence(s: &[Token], state: &mut ParseState) {
    let (def_part, main_part) = split_at_first(s, Token::Is);

    match (def_part.len(), &def_part[0]) {
        (1, Token::TypeName(ty)) => {
            todo!();
            // pattern def
            let (pattern, condition) = split_at_first(s, Token::If);
        }
        _ => {
            // func
            let signature = parse_func_def(def_part, state);
            let f = parse_expr(main_part, state);
            state.insert_func(signature, FuncContent::Custom(f));
        }
    }
}

fn parse_func_def(s: &[Token], state: &mut ParseState) -> FuncSignature {
    let mut args = Vec::new();
    if let Token::TypeName(t) = &s[0] {
        args.push(state.get_type(SymbolType::from(*t)));
    }
    let mut i = args.len(); // is start arg: 1, else 0
    let mut name = Vec::new();
    while let Some(Token::Symbol(s)) = s.get(i) {
        name.push(SymbolType::from(*s));
        i += 1;
    }

    if !args.is_empty() {
        if let Token::TypeName(t) = &s[0] {
            args.push(state.get_type(SymbolType::from(*t)));
        }
    }

    FuncSignature {
        info: FuncInfo { args },
        name,
        //args: todo!(),
    }
}

fn parse_expr<'a>(s: &'a [Token], state: &'a mut ParseState) -> Expr {
    let mut tokens = s.iter();
    let mut current_expr = match tokens.next().unwrap() {
        Token::The(a) => Expr::The(state.get_type(SymbolType::from(*a))),
        Token::Number(n) => Expr::Number(*n),
        Token::StringLiteral(s) => Expr::Str(SymbolType::from(*s)),
        Token::Symbol(_) => todo!(),
        Token::TypeName(_) => todo!(),
        a => panic!("unexpected {:?}", a),
    };

    loop {
        current_expr = match tokens.next() {
            Some(Token::Symbol(s)) => {
                let mut signature = vec![SymbolType::from(*s)];

                if let Some(mut b) = state.func_names.get(*s) {
                    loop {
                        match b {
                            &FuncTree::Func(ptr) => {
                                let mut args = vec![current_expr];
                                if state.func_map[ptr.0].0.args.len() == 2 {
                                    args.push(parse_expr(
                                        &tokens.clone().cloned().collect::<Vec<_>>(),
                                        state,
                                    ));
                                    return Expr::Call { func: ptr, args };
                                }
                                break Expr::Call { func: ptr, args };
                            }
                            FuncTree::Branches(new_map) => match tokens.next() {
                                Some(Token::Symbol(s)) => {
                                    signature.push(SymbolType::from(*s));
                                    match new_map.get(*s) {
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
            Some(a) => panic!("unexpected {:?}", a),
            None => return current_expr,
        };
    }
}
