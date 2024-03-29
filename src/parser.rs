use core::panic;
use std::collections::HashMap;

use logos::Logos;

use crate::interpreter::Value;
use internment::LocalIntern;
pub type Symbol = LocalIntern<String>;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    //keywords
    #[token("the")]
    The,

    #[token("if")]
    If,

    #[token("otherwise")]
    Otherwise,

    #[token("with")]
    With,

    #[token("each")]
    Each,

    #[token("item")]
    Item,

    #[token("in")]
    In,

    #[token("that")]
    That,

    #[token("is")]
    Is,

    #[token("where")]
    Where,

    #[token(".")]
    Period,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("not")]
    Not,

    #[token("of")]
    Of,

    #[token("list")]
    List,

    #[token("new")]
    New,

    #[regex(r"[0-9]+(\.[0-9]+)?", | lex | lex.slice().parse::<f64>().unwrap())]
    Number(f64),

    #[regex(r#"[a-z]?"(?:\\.|[^\\"])*"|'(?:\\.|[^\\'])*'"#, | lex | {
        let slice = lex.slice();
        Symbol::from(&slice[1..(slice.len() - 1)])
    })]
    StringLiteral(Symbol),

    // list as a type is capitalized for now lol
    #[regex(r#"number|string|List"#, | lex | Symbol::from(lex.slice()))]
    TypeName(Symbol),

    #[regex(r"([a-zA-Z_][a-zA-Z0-9_]*)|\$", | lex | Symbol::from(lex.slice()))]
    Symbol(Symbol),

    #[error]
    #[regex(r"[ \t\f\n\r]+|/\*[^*]*\*(([^/\*][^\*]*)?\*)*/|//[^\n]*", logos::skip)]
    Error,
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum Token {
//     The(Symbol),
//     If,
//     Is,
//     Period,
//     Colon,
//     Comma,
//     And,
//     Or,
//     Not,
//     Of,
//     List,
//     Otherwise,
//     Number(f64),
//     StringLiteral(Symbol),
//     Symbol(Symbol),
//     TypeName(Symbol),
// }

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

    pub fn previous(&mut self) {
        self.index -= 1;
    }
}

pub fn lex(text: &str) -> Vec<Token> {
    Token::lexer(text).collect::<Vec<_>>()
}

#[derive(PartialEq, Clone, Debug, Copy)]
pub struct NamedFunc {
    pub pattern: Option<PatPtr>,
    pub perfectum: Option<FnPtr>,
    pub property: Option<PropPtr>,
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

        match current_branch
            .entry(*symbols.last().unwrap())
            .or_insert(WordTree::Leaf(NamedFunc {
                pattern: None,
                perfectum: None,
                property: None,
            })) {
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

#[derive(PartialEq, Clone, Copy, Hash, Eq, Debug)]
pub struct PropPtr(pub usize);

#[derive(Debug)]
pub struct ParseState {
    pub names: WordTree,
    type_count: u32,
    pub type_map: HashMap<Symbol, Ty>,
    pub func_map: Vec<(FuncInfo, FuncContent)>,
    pub prop_map: Vec<(Ty, PropContent)>,
    pub pat_map: Vec<(FuncInfo, PatContent)>,
    pub scope_args: Vec<ScopeArg>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ScopeArg {
    Typed(Ty, Option<Symbol>),
    Item,
}

use crate::builtin::{BUILTIN_FUNCS, BUILTIN_PATTERNS, BUILTIN_PROPS};

#[derive(Debug, Clone)]
pub enum FuncContent {
    Custom(Expr),
    Builtin(fn(Vec<Value>) -> Value),
    Uninitialized,
}

#[derive(Debug, Clone)]
pub enum PropContent {
    Custom(Expr),
    Builtin(fn(Value) -> Value),
    Uninitialized,
}

#[derive(Debug, Clone)]
pub enum PatContent {
    Custom(PatternExpr),
    Builtin(fn(Vec<Value>) -> bool),
    Uninitialized,
}

impl ParseState {
    pub fn new() -> Self {
        let mut out = Self {
            names: WordTree::new(),
            type_count: 0,
            type_map: HashMap::new(),
            func_map: Vec::new(),
            prop_map: Vec::new(),
            scope_args: Vec::new(),

            pat_map: Vec::new(),
        };
        for (name, types, f) in BUILTIN_FUNCS {
            let signature = FuncSignature {
                info: FuncInfo {
                    args: types
                        .iter()
                        .map(|a| (out.get_type(Symbol::from(*a)), None))
                        .collect(),
                },
                name: name.iter().map(|a| Symbol::from(*a)).collect(),
            };

            let index = out.insert_func(signature);
            out.func_map[index.0].1 = FuncContent::Builtin(*f);
        }

        for (name, types, f) in BUILTIN_PATTERNS {
            let signature = FuncSignature {
                info: FuncInfo {
                    args: types
                        .iter()
                        .map(|a| (out.get_type(Symbol::from(*a)), None))
                        .collect(),
                },
                name: name.iter().map(|a| Symbol::from(*a)).collect(),
            };
            let index = out.insert_pat(signature);
            out.pat_map[index.0].1 = PatContent::Builtin(*f);
        }

        for (name, types, f) in BUILTIN_PROPS {
            let name = name.iter().map(|a| Symbol::from(*a)).collect();
            let typ = out.get_type(Symbol::from(*types));
            let index = out.insert_prop(typ, name);
            out.prop_map[index.0].1 = PropContent::Builtin(*f);
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
        self.scope_args = func
            .info
            .args
            .iter()
            .copied()
            .map(|(a, b)| ScopeArg::Typed(a, b))
            .collect();
    }

    fn insert_func(&mut self, signature: FuncSignature) -> FnPtr {
        (*self.names.access(signature.name)).perfectum = Some(FnPtr(self.func_map.len()));
        self.func_map
            .push((signature.info, FuncContent::Uninitialized));
        FnPtr(self.func_map.len() - 1)
    }

    fn insert_pat(&mut self, signature: FuncSignature) -> PatPtr {
        (*self.names.access(signature.name)).pattern = Some(PatPtr(self.pat_map.len()));
        self.pat_map
            .push((signature.info, PatContent::Uninitialized));
        PatPtr(self.pat_map.len() - 1)
    }

    fn insert_prop(&mut self, typ: Ty, name: Vec<Symbol>) -> PropPtr {
        (*self.names.access(name)).property = Some(PropPtr(self.prop_map.len()));
        self.prop_map.push((typ, PropContent::Uninitialized));
        PropPtr(self.prop_map.len() - 1)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Call {
    pub func: FnPtr,
    pub args: Vec<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Number(f64),
    Str(Symbol),
    Call(Call),
    If {
        condition: PatternExpr,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },
    ArgRef(usize),
    List(Vec<Expr>),
    ListMap {
        list: Box<Expr>,
        func: Call,
    },
    ListFilter {
        list: Box<Expr>,
        predicate: PatternExpr,
    },
    Prop {
        arg: Box<Expr>,
        func: PropPtr,
    },
}

#[derive(PartialEq, Clone, Debug)]
pub struct Match {
    pub pat: PatPtr,
    pub args: Vec<Expr>,
    pub not: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub enum PatternExpr {
    Match(Match),
    And(Box<PatternExpr>, Box<PatternExpr>),
    Or(Box<PatternExpr>, Box<PatternExpr>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct FuncInfo {
    args: Vec<(Ty, Option<Symbol>)>,
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
            a => panic!(concat!("Expected ", stringify!($token), ", found {:?}"), a),
        }
    };
}

macro_rules! expect_ret {
    ($token:ident, $tokens:expr) => {
        match $tokens.next() {
            Some(Token::$token(a)) => a,
            a => panic!(concat!("Expected ", stringify!($token), ", found {:?}"), a),
        }
    };
}

// fn split_at_first<'a>(tokens: &'a [Token], t: Token) -> (&'a [Token<'a>], &'a [Token<'a>]) {
//     let out = tokens.split_at(tokens.iter().position(|a| *a == t).unwrap());
//     (out.0, &out.1[1..])
// }

fn parse_sentence(tokens: &mut Tokens, state: &mut ParseState) {
    let backup = tokens.index;
    let first_type = tokens.next();

    match (first_type, tokens.next()) {
        (Some(Token::TypeName(t)), Some(Token::Is)) => {
            let first_type = state.get_type(t);

            let arg_name = check_for_name(tokens);

            let mut def_part = Vec::new();
            loop {
                match tokens.next() {
                    Some(Token::If) => break,
                    Some(a) => def_part.push(a),
                    None => panic!("expected symbol or \"if\""),
                }
            }

            let signature = parse_pat_def(&def_part, state, first_type, arg_name);
            let index = state.insert_pat(signature.clone());
            state.set_scope_args(&signature);
            let condition = parse_pattern_expr(tokens, state);

            state.pat_map[index.0].1 = PatContent::Custom(condition);
        }
        (Some(Token::The), _) => {
            tokens.previous();
            let mut def_part = Vec::new();
            loop {
                match tokens.next() {
                    Some(Token::Of) => break,
                    Some(a) => def_part.push(a),
                    None => panic!("expected symbol or \"of\""),
                }
            }

            let name = parse_prop_def(&def_part);
            let type_name = expect_ret!(TypeName, tokens);
            let typ = state.get_type(type_name);

            let arg_name = check_for_name(tokens);
            let index = state.insert_prop(typ, name.clone());
            state.set_scope_args(&FuncSignature {
                info: FuncInfo {
                    args: vec![(typ, arg_name)],
                },
                name,
            });

            expect!(Token::Is, tokens);
            let f = parse_value(tokens, state, ExprParseAmount::Exhaustive, true);
            state.prop_map[index.0].1 = PropContent::Custom(f);
        }
        _ => {
            tokens.index = backup;

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
            //dbg!(signature.clone(), def_part);
            let index = state.insert_func(signature.clone());
            state.set_scope_args(&signature);
            let f = parse_value(tokens, state, ExprParseAmount::Exhaustive, true);
            state.func_map[index.0].1 = FuncContent::Custom(f);
        }
    };

    expect!(Token::Period, tokens);
}

fn check_for_name(tokens: &mut Tokens) -> Option<Symbol> {
    if let Some(Token::OpenParen) = tokens.next() {
        let name = expect_ret!(Symbol, tokens);
        expect!(Token::CloseParen, tokens);
        Some(name)
    } else {
        tokens.previous();
        None
    }
}

fn parse_prop_def(def_part: &[Token]) -> Vec<Symbol> {
    let mut tokens = Tokens::new(def_part);
    let mut name = Vec::new();
    while let Some(Token::Symbol(s)) = tokens.next() {
        name.push(s);
    }
    name
}

fn parse_func_def(s: &[Token], state: &mut ParseState) -> FuncSignature {
    let mut args = Vec::new();
    let mut tokens = Tokens::new(s);

    if let Some(Token::TypeName(t)) = &tokens.next() {
        args.push((state.get_type(*t), check_for_name(&mut tokens)));
    } else {
        tokens.previous();
    }

    let mut name = Vec::new();
    while let Some(Token::Symbol(s)) = tokens.next() {
        name.push(s);
    }

    if !args.is_empty() {
        if let Some(Token::TypeName(t)) = &tokens.next() {
            args.push((state.get_type(*t), check_for_name(&mut tokens)));
        } else {
            tokens.previous();
        }
    }

    FuncSignature {
        info: FuncInfo { args },
        name,
        //args: todo!(),
    }
}

fn parse_pat_def(
    s: &[Token],
    state: &mut ParseState,
    first_type: Ty,
    arg_name: Option<Symbol>,
) -> FuncSignature {
    let mut tokens = Tokens::new(s);
    let mut args = vec![(first_type, arg_name)];
    let mut name = Vec::new();
    while let Some(Token::Symbol(s)) = tokens.next() {
        name.push(s);
    }

    if !args.is_empty() {
        if let Some(Token::TypeName(t)) = tokens.next() {
            args.push((state.get_type(t), check_for_name(&mut tokens)));
        } else {
            tokens.previous();
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

fn parse_value(
    tokens: &mut Tokens,
    state: &mut ParseState,
    amount: ExprParseAmount,
    allow_colon: bool,
) -> Expr {
    #![warn(unused_parens)]
    let exhaustive = amount == ExprParseAmount::Exhaustive
        || if tokens.next() == Some(Token::Colon) && allow_colon {
            true
        } else {
            tokens.previous();
            false
        };
    let current_expr = match tokens.next() {
        Some(Token::The) => match tokens.next() {
            Some(Token::TypeName(a)) => {
                let typ = state.get_type(a);
                let mut list = state.scope_args.iter().enumerate().filter_map(|(i, a)| {
                    if let ScopeArg::Typed(t, _) = *a {
                        if t == typ {
                            Some(i)
                        } else {
                            None
                        }
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
            Some(Token::Item) => {
                let mut list = state.scope_args.iter().enumerate().filter_map(|(i, a)| {
                    if *a == ScopeArg::Item {
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
            Some(Token::Symbol(_)) => {
                tokens.previous();
                let ptr = parse_multi_symbol_name(state, tokens);
                expect!(Token::Of, tokens);
                let val = parse_value(tokens, state, ExprParseAmount::Value, true);
                let ptr = ptr.property.expect("no function with this name");
                Expr::Prop {
                    arg: val.into(),
                    func: ptr,
                }
            }
            a => panic!("expected typename, got {:?}", a),
        },
        Some(Token::Number(n)) => (Expr::Number(n)),
        Some(Token::StringLiteral(s)) => (Expr::Str(s)),
        Some(Token::Symbol(symbol)) => {
            // check if it's an arg name
            let mut list = state.scope_args.iter().enumerate().filter_map(|(i, a)| {
                if let ScopeArg::Typed(_, Some(n)) = *a {
                    if n == symbol {
                        Some(i)
                    } else {
                        None
                    }
                } else {
                    None
                }
            });

            if let Some(index) = list.next() {
                if list.next() != None {
                    panic!("ambiguous arg ref")
                }
                Expr::ArgRef(index)
            } else {
                tokens.previous();
                return parse_expression(None, tokens, state);
            }
        }
        Some(Token::TypeName(_)) => todo!(),
        Some(Token::New) => {
            expect!(Token::List, tokens);
            Expr::List(Vec::new())
        }
        Some(Token::List) => {
            expect!(Token::Of, tokens);
            match tokens.next() {
                Some(Token::Each) => {
                    // filter
                    expect!(Token::Item, tokens);
                    state.scope_args.push(ScopeArg::Item);
                    match tokens.next() {
                        Some(Token::That) => {
                            expect!(Token::Is, tokens);
                            let predicate = parse_match(
                                Some(Expr::ArgRef(state.scope_args.len() - 1)),
                                tokens,
                                state,
                            );
                            expect!(Token::In, tokens);
                            let list = parse_value(tokens, state, ExprParseAmount::Value, false);

                            Expr::ListFilter {
                                list: list.into(),
                                predicate: PatternExpr::Match(predicate),
                            }
                        }

                        Some(Token::In) => {
                            let list = parse_value(tokens, state, ExprParseAmount::Value, false);
                            expect!(Token::Where, tokens);
                            let pat = parse_pattern_expr(tokens, state);

                            Expr::ListFilter {
                                list: list.into(),
                                predicate: pat,
                            }
                        }

                        _ => panic!("expected `that` or `in`"),
                    }
                }
                _ => {
                    tokens.previous();
                    // list literal
                    let mut list = Vec::new();
                    list.push(parse_value(tokens, state, ExprParseAmount::Value, false));
                    if let Some(Token::And) | Some(Token::Comma) = tokens.next() {
                        tokens.previous();

                        loop {
                            match tokens.next() {
                                Some(Token::Comma) => match tokens.next() {
                                    Some(Token::And) => {
                                        list.push(parse_value(
                                            tokens,
                                            state,
                                            ExprParseAmount::Value,
                                            false,
                                        ));
                                        break;
                                    }
                                    _ => tokens.previous(),
                                },
                                Some(Token::And) => {
                                    list.push(parse_value(
                                        tokens,
                                        state,
                                        ExprParseAmount::Value,
                                        false,
                                    ));
                                    break;
                                }
                                _ => panic!("expected comma or `and`"),
                            }
                            list.push(parse_value(tokens, state, ExprParseAmount::Value, false));
                        }
                    } else {
                        tokens.previous();
                    }

                    // expect!((Token::Comma | Token::Period), tokens);
                    // tokens.previous();
                    Expr::List(list)
                }
            }
        }
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
    'expr_loop: loop {
        current_expr = Some(match tokens.next() {
            Some(Token::Symbol(symbol)) => {
                // if current_expr == None {
                //     let mut list = state.scope_args.iter().enumerate().filter_map(|(i, a)| {
                //         if let ScopeArg::Typed(_, Some(n)) = *a {
                //             if n == symbol {
                //                 Some(i)
                //             } else {
                //                 None
                //             }
                //         } else {
                //             None
                //         }
                //     });

                //     dbg!(symbol, &list);

                //     if let Some(index) = list.next() {
                //         if list.next() != None {
                //             panic!("ambiguous arg ref")
                //         }
                //         current_expr = Some(Expr::ArgRef(index));
                //         continue 'expr_loop;
                //     }
                // }
                tokens.previous();
                let mut args = current_expr.into_iter().collect::<Vec<_>>();

                let ptr = parse_multi_symbol_name(state, tokens);

                let ptr = ptr.perfectum.expect("no function with this name");
                if state.func_map[ptr.0].0.args.len() == 2 {
                    args.push(parse_value(tokens, state, ExprParseAmount::Value, true));
                }
                Expr::Call(Call { func: ptr, args })
            }
            Some(Token::Comma) => {
                if let Some(e) = current_expr {
                    parse_expression(Some(e), tokens, state)
                } else {
                    panic!("unexpected comma")
                }
            }
            Some(Token::If) => {
                let condition = parse_pattern_expr(tokens, state);
                match tokens.next() {
                    Some(Token::Comma) => (),
                    _ => {
                        tokens.previous();
                        tokens.previous();
                        match tokens.next() {
                            Some(Token::Comma) => (),
                            _ => panic!("expected comma"),
                        }
                    }
                };
                expect!(Token::Otherwise, tokens);
                let otherwise = parse_value(tokens, state, ExprParseAmount::Exhaustive, true);
                Expr::If {
                    condition,
                    then: current_expr.unwrap().into(),
                    otherwise: otherwise.into(),
                }
            }
            Some(Token::With) => {
                expect!(Token::Each, tokens);
                expect!(Token::Item, tokens);

                let ptr = parse_multi_symbol_name(state, tokens)
                    .perfectum
                    .expect("no function with this name");

                let args = if state.func_map[ptr.0].0.args.len() == 2 {
                    vec![parse_value(tokens, state, ExprParseAmount::Value, true)]
                } else {
                    Vec::new()
                };

                Expr::ListMap {
                    list: current_expr.expect("expected list").into(),
                    func: Call { func: ptr, args },
                }
            }
            Some(a) => {
                tokens.previous();
                return current_expr.unwrap_or_else(|| panic!("unexpected {:?}", a));
            }
            None => panic!("unexpected end of input"),
        });
    }
}

fn parse_multi_symbol_name(state: &mut ParseState, tokens: &mut Tokens) -> NamedFunc {
    let mut signature = Vec::new();
    let start_symbol = if let Some(Token::Symbol(s)) = tokens.next() {
        s
    } else {
        panic!("expected symbol")
    };
    if let Some(mut b) = state.names.branches().get(&start_symbol) {
        loop {
            match b {
                &WordTree::Leaf(ptr) => break ptr,
                WordTree::Branches(new_map) => {
                    let s = match tokens.next() {
                        Some(Token::Symbol(s)) => s,
                        Some(Token::With) => Symbol::from("with"),
                        Some(Token::Item) => Symbol::from("item"),
                        a => panic!("expected symbol, found: {:?}", a),
                    };
                    signature.push(s);
                    match new_map.get(&s) {
                        Some(b2) => b = b2,
                        None => panic!("undefined function: {:?}", signature),
                    }
                }
            }
        }
    } else {
        panic!("undefined function start: {}", start_symbol)
    }
}

fn parse_pattern_expr(tokens: &mut Tokens, state: &mut ParseState) -> PatternExpr {
    let value = parse_value(tokens, state, ExprParseAmount::Exhaustive, true);
    expect!(Token::Is, tokens);
    PatternExpr::Match(parse_match(Some(value), tokens, state))
}

fn parse_match(value: Option<Expr>, tokens: &mut Tokens, state: &mut ParseState) -> Match {
    let not = if let Some(Token::Not) = tokens.next() {
        true
    } else {
        tokens.previous();
        false
    };

    let mut args = value.into_iter().collect::<Vec<_>>();
    let ptr = parse_multi_symbol_name(state, tokens)
        .pattern
        .expect("no pattern with this name");

    if state.pat_map[ptr.0].0.args.len() == 2 {
        args.push(parse_value(
            tokens,
            state,
            ExprParseAmount::Exhaustive,
            true,
        ));
    }

    Match {
        pat: ptr,
        args,
        not,
    }
}
