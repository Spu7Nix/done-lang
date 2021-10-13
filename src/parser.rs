use std::collections::HashMap;

use logos::Logos;

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
    #[regex(r"[ \t\f]+|/\*[^*]*\*(([^/\*][^\*]*)?\*)*/|//[^\n]*", logos::skip)]
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
pub enum Expr {
    Number(f64),
    Str(String),
    Call {
        func: FuncSignature,
        args: Vec<Expr>
    },
    The(String)
}



#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum FuncSymbol {
    Argument,
    Symbol(String)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct FuncSignature(Vec<FuncSymbol>);
#[derive(Debug)]
pub struct Functions(HashMap<FuncSignature, Expr>);

pub fn parse_mod(tokens: &[Token]) -> Functions {
    let mut funcs = Functions(HashMap::new());
    let sentences = tokens.split(|a| *a == Token::Period);
    for s in sentences {
        if !s.is_empty() {
            parse_sentence(s, &mut funcs);
        }
    }
    funcs
}

fn split_at_first<'a>(tokens: &'a [Token], t: Token) -> (&'a [Token<'a>], &'a [Token<'a>]) {
    let out = tokens.split_at(tokens.iter().position(|a | *a == t).unwrap());
    (out.0, &out.1[1..])
}

fn parse_sentence(s: &[Token], funcs: &mut Functions) {
    let (def_part, main_part) = split_at_first(s, Token::Is);

    match (def_part.len(), def_part[0]) {
        (1, Token::TypeName(ty)) => {
            todo!();
            // pattern def
            let (pattern, condition) = split_at_first(s, Token::If);
        },
        _ => {
            // func
            let signature = parse_multi_word_def(def_part);
            let f = parse_expr(main_part, funcs);
            funcs.0.insert(signature.clone(), f);
        }
    }
}

fn parse_multi_word_def<'a>(s: &'a [Token]) -> FuncSignature {
    FuncSignature(s.iter().map(|a| match a {
        Token::Symbol(s) => FuncSymbol::Symbol(s.to_string()),
        Token::TypeName(_) => FuncSymbol::Argument,
        _ => panic!("unexpected")
    }).collect())
}



fn parse_expr<'a>(s: &'a [Token], funcs: &'a Functions) -> Expr {
    
    match s[0] {
        Token::The(a) => Expr::The(a.to_string()),
        Token::Number(n) => Expr::Number(n),
        Token::StringLiteral(s) => Expr::Str(s.to_string()),
        Token::Symbol(_) => todo!(),
        Token::TypeName(_) => todo!(),
        a => panic!("unexpected {:?}", a)
    }
}