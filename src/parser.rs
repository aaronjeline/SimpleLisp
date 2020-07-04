use std::rc::Rc;
use logos::Logos;
use crate::lex::Token;
use crate::expr::*;
use crate::expr::Expr::*;

// Core parsing type
type ParseResult = Option<(Expr, Vec<Token>)>;

// Top level parsing function
pub fn parse(src: &str) -> Option<Expr> { 
    // Tokenize the strings
    let mut toks:Vec<_> = Token::lexer(src).collect();
    // String is reverse to allow use of push/pop functions because im lazy
    toks.reverse();

    let (e, toks) = parse_expr(toks)?;
    // Parsing should consume all tokens
    if toks.len() == 0 { 
        Some(e)
    } else { 
        None
    }
}


fn parse_expr(mut toks: Vec<Token>) -> ParseResult { 
    match toks.pop()? { 
        Token::Number(i) => Some((Int(i), toks)),
        Token::Symbol(s) => Some((Symbol(s), toks)),
        Token::Str(s) => Some((Str(s), toks)),
        Token::OParen => parse_list(toks),
        Token::Quote => reader_expand(toks, "quote"),
        Token::Unquote => reader_expand(toks, "unquote"),
        Token::Quasiquote => reader_expand(toks, "quasiquote"),
        _ => None
    }

}

fn reader_expand(mut toks: Vec<Token>, name: &str) -> ParseResult {
    let (quoted, toks) = parse_expr(toks)?;
    let lst = Cons(Rc::new(Symbol(name.to_string())),
                   Rc::new(Cons(Rc::new(quoted),
                                Rc::new(Nil))));

    Some((lst, toks))
    
}



fn parse_list(mut toks: Vec<Token>) -> ParseResult { 
    match toks.get(toks.len() - 1)? { 
        Token::CParen => { 
                            toks.pop();
                            Some((Nil, toks))
        },
        _ => { 
                    let (car, toks) = parse_expr(toks)?;
                    let (cdr, toks) = parse_list(toks)?;
                    Some((Cons(Rc::new(car), Rc::new(cdr)), toks))
        }
    }
}


