use logos::Logos;
use num_bigint::BigInt;
use std::fmt;

// Lexer, almost entirely derived by macros

#[derive(Logos, PartialEq, Clone, Debug)]
pub enum Token {

    #[token("(")]
    OParen,

    #[token(")")]
    CParen,

    #[token("'")]
    Quote,

    #[token("`")]
    Quasiquote,

    #[token(",")]
    Unquote,

    #[regex("[!a-z+\\-*?][!a-z\\-+*?0-9]*", |lex| lex.slice().to_string())]
    Symbol(String),

    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Number(BigInt),

    #[regex("\"[^\"\\\\]*(\\\\.[^\"\\\\]*)*\"", |lex| strip_quotes(lex.slice().to_string()))]
    Str (String),


    #[error]
    #[regex(r"[ \t\n\f ]+", logos::skip)]
    Error,
}

fn strip_quotes(mut s: String) -> String { 
    s.pop();
    s.remove(0);
    s
}

use Token::*;

