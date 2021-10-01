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

    #[regex(r#"\w+"#, priority = 0)]
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
}

pub fn lex(text: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut iter = LogosToken::lexer(text);
    while let Some(token) = iter.next() {
        tokens.push(match token {
            LogosToken::The => Token::The({
                assert_eq!(iter.next().unwrap(), LogosToken::Symbol);
                iter.slice()
            }),
            LogosToken::If => Token::If,
            LogosToken::Is => Token::Is,
            LogosToken::Period => Token::Period,
            LogosToken::Define => Token::Define,
            LogosToken::Comma => Token::Comma,
            LogosToken::Number => Token::Number(iter.slice().parse().unwrap()),
            LogosToken::StringLiteral => Token::StringLiteral(iter.slice()),
            LogosToken::Symbol => Token::Symbol(iter.slice()),
            LogosToken::Error => todo!(),
        })
    }
    tokens
}
