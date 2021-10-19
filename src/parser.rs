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

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("not")]
    Not,

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
    And,
    Or,
    Not,
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
            LogosToken::And => Token::And,
            LogosToken::Or => Token::Or,
            LogosToken::Not => Token::Not,
        })
    }
    tokens
}

#[derive(PartialEq, Clone, Debug, Copy)]
pub struct NamedFunc {
    pub pattern: Option<PatPtr>,
    pub perfectum: Option<FnPtr>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum WordTree {
    Leaf(NamedFunc),
    Branches(HashMap<Symbol, WordTree>),
}

impl WordTree {
    fn new() -> WordTree {
        WordTree::Branches(HashMap::new())
    }

    pub fn mut_branches(&mut self) -> &mut HashMap<Symbol, WordTree> {
        match self {
            WordTree::Leaf(_) => unreachable!(),
            WordTree::Branches(b) => b,
        }
    }
    pub fn branches(&self) -> &HashMap<Symbol, WordTree> {
        match self {
            WordTree::Leaf(_) => unreachable!(),
            WordTree::Branches(b) => b,
        }
    }

    pub fn access(&mut self, symbols: Vec<Symbol>) -> &mut NamedFunc {
        let mut current_branch = self.mut_branches();
        for name in &symbols[..symbols.len() - 1] {
            current_branch = match current_branch
                .entry(*name)
                .or_insert_with(|| WordTree::Branches(HashMap::new()))
            {
                WordTree::Leaf(_) => panic!("Name already exists"),
                WordTree::Branches(b) => b,
            };
        }
        
        match current_branch.entry(*symbols.last().unwrap()).or_insert(WordTree::Leaf(NamedFunc { pattern: None, perfectum: None })) {
            WordTree::Leaf(l) => l,
            WordTree::Branches(_) => panic!("name too short"),
        }
    }
}
#[derive(PartialEq, Clone, Copy, Hash, Eq, Debug)]
pub struct Ty(u32);

#[derive(PartialEq, Clone, Copy, Hash, Eq, Debug)]
pub struct FnPtr(pub usize);

#[derive(PartialEq, Clone, Copy, Hash, Eq, Debug)]
pub struct PatPtr(pub usize);
#[derive(Debug)]
pub struct ParseState {
    pub names: WordTree,
    type_count: u32,
    pub type_map: HashMap<Symbol, Ty>,
    pub func_map: Vec<(FuncInfo, FuncContent)>,
    pub pat_map: Vec<(FuncInfo, PatContent)>,
    pub scope_args: Vec<Ty>,
}

use crate::builtin::{BUILTIN_FUNCS, BUILTIN_PATTERNS};

#[derive(Debug, Clone)]
pub enum FuncContent {
    Custom(Expr),
    Builtin(fn(Vec<Value>) -> Value),
}

#[derive(Debug, Clone)]
pub enum PatContent {
    Custom(PatternExpr),
    Builtin(fn(Vec<Value>) -> bool),
}

impl ParseState {
    pub fn new() -> Self {
        let mut out = Self {
            names: WordTree::new(),
            type_count: 0,
            type_map: HashMap::new(),
            func_map: Vec::new(),
            scope_args: Vec::new(),
            
            pat_map: Vec::new(),
        };
        for (name, types, f) in BUILTIN_FUNCS {
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

        for (name, types, f) in BUILTIN_PATTERNS {
            let signature = FuncSignature {
                info: FuncInfo {
                    args: types
                        .iter()
                        .map(|a| out.get_type(Symbol::from(*a)))
                        .collect(),
                },
                name: name.iter().map(|a| Symbol::from(*a)).collect(),
            };

            out.insert_pat(signature, PatContent::Builtin(*f))
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
        self.func_map.push((signature.info, f));
        
            (*self.names
                .access(signature.name)).perfectum = Some(FnPtr(self.func_map.len() - 1));
    }

    fn insert_pat(&mut self, signature: FuncSignature, f: PatContent) {
        self.pat_map.push((signature.info, f));
        (*self.names
            .access(signature.name)).pattern = Some(PatPtr(self.pat_map.len() - 1));
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Number(f64),
    Str(Symbol),
    Call {
        func: FnPtr,
        args: Vec<Expr>,
    },
    If {
        condition: PatternExpr,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },
    ArgRef(usize),
}

#[derive(PartialEq, Clone, Debug)]
pub enum PatternExpr {
    Match { pat: PatPtr, args: Vec<Expr> },
    Not(Box<PatternExpr>),
    And(Box<PatternExpr>, Box<PatternExpr>),
    Or(Box<PatternExpr>, Box<PatternExpr>),
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

macro_rules! expect {
    ($token:pat, $tokens:expr) => {
        match $tokens.next() {
            Some($token) => (),
            a => panic!(concat!("Expected", stringify!($token), ", found {:?}"), a),
        }
    };
}

// fn split_at_first<'a>(tokens: &'a [Token], t: Token) -> (&'a [Token<'a>], &'a [Token<'a>]) {
//     let out = tokens.split_at(tokens.iter().position(|a| *a == t).unwrap());
//     (out.0, &out.1[1..])
// }

fn parse_sentence(tokens: &mut Tokens, state: &mut ParseState) {
    let first_type = tokens.next();

    match (first_type, tokens.next()) {
        (Some(Token::TypeName(t)), Some(Token::Is)) => {
            let first_type = state.get_type(t);
            let mut def_part = Vec::new();
            loop {
                match tokens.next() {
                    Some(Token::If) => break,
                    Some(a) => def_part.push(a),
                    None => panic!("expected type name, symbol or \"is\""),
                }
            }
            let signature = parse_pat_def(&def_part, state, first_type);
            state.set_scope_args(&signature);
            let condition = parse_pattern_expr(tokens, state);
            dbg!(&signature, &condition);
            state.insert_pat(signature, PatContent::Custom(condition));
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

    expect!(Token::Period, tokens);
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

fn parse_pat_def(s: &[Token], state: &mut ParseState, first_type: Ty) -> FuncSignature {
    let mut args = vec![first_type];
    let mut i = 0;
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
                tokens.previous();
                let mut args = current_expr.into_iter().collect::<Vec<_>>();

                let ptr = parse_multi_symbol_name(state, tokens).perfectum.expect("no function with this name");

                if state.func_map[ptr.0].0.args.len() == 2 {
                    args.push(parse_value(tokens, state, ExprParseAmount::Value)); 
                }
                Expr::Call { func: ptr, args }
            }
            Some(Token::Comma) => {
                if let Some(e) = current_expr {
                    parse_expression(Some(e), tokens, state)
                } else {
                    panic!("unexpected comma")
                }
            }
            Some(_) => {
                tokens.previous();
                return current_expr.unwrap();
            }
            None => panic!("unexpected end of input"),
        });
    }
}

fn parse_multi_symbol_name(state: &mut ParseState, tokens: &mut Tokens) -> NamedFunc {
    let mut signature = Vec::new();
    let start_symbol = if let Some(Token::Symbol(s)) = tokens.next() {
        s
    } else { panic!("expected symbol")};
    if let Some(mut b) = state.names.branches().get(&start_symbol) {
        loop {
            match b {
                &WordTree::Leaf(ptr) => {
                    
                    break ptr
                }
                WordTree::Branches(new_map) => match tokens.next() {
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
        panic!("undefined function start: {}", start_symbol)
    }
}

fn parse_pattern_expr(tokens: &mut Tokens, state: &mut ParseState) -> PatternExpr {
    let value = parse_value(tokens, state, ExprParseAmount::Exhaustive);
    expect!(Token::Is, tokens);
    let mut args = vec![value];

    let ptr = parse_multi_symbol_name(state, tokens).pattern.expect("no pattern with this name");

    if state.pat_map[ptr.0].0.args.len() == 2 {
        args.push(parse_value(tokens, state, ExprParseAmount::Exhaustive)); 
    }
    PatternExpr::Match { pat: ptr, args }
}
