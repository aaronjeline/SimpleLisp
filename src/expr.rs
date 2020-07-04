use num_bigint::BigInt;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{LinkedList, HashMap};
use std::fmt;

// Two result types
pub type Result<T> = core::result::Result<T, EvalError>;
pub type EResult = Result<Rc<Expr>>;

const KEYWORDS: [&'static str; 9] = 
    ["if", "let", "fun", "quote", "define", "begin", "unquote", "quasiquote", "catch"];

// Core Expression Form
pub enum Expr { 
    Int (BigInt),
    Str (String),
    Symbol (String),
    Cons (Rc<Expr>, Rc<Expr>),
    Closure (Vec<String>, Rc<Expr>, Rc<Env>),
    Nil,
    Builtin (fn(Vec<Rc<Expr>>) -> EResult), // Native calls
    Dummy, // Used for temporary recursive bindings
}
use Expr::*;

// Errors that can arise from execution
#[derive(Clone, Debug)]
pub enum EvalError { 
    ArityError(usize, usize),
    LetError,
    IfError,
    ClosureError,
    ApplicationNonFunction,
    DefineError,
    UnboundSymbol (String),
    TypeError(&'static str, &'static str, &'static str),
    Generic(String),
    UnQuoteOutsideQuote,
    DummyError,
}
use EvalError::*;


// Structure for environments
pub struct Env { 
    data: RefCell<HashMap<String, RefCell<Rc<Expr>>>>,
    next: Option<Rc<Env>>, 
}


///////////////////////////////////////////////////////////////////////////////
// Implementations
///////////////////////////////////////////////////////////////////////////////

impl Expr { 

    // Is an expr a list?
    pub fn is_list(&self) -> bool { 
        match self { 
            Cons (_,cdr) => cdr.is_list(),
            Nil => true,
            _ => false,
        }
    }

    pub fn get_type(&self) -> &'static str {
        match self { 
            Int(_) => "int",
            Str(_) => "string",
            Symbol(_) => "symbol",
            Cons(_,_) => "cons",
            Closure(_,_,_) => "closure",
            Nil => "nil",
            Builtin(_) => "builtin procedure",
            Dummy => "dummy type",
        }
    }

    // Pull an expr into a list
    pub fn collapse(&self) -> Option<Vec<Rc<Expr>>> { 
        match self { 
            Nil => Some(vec![]),
            Cons(car,cdr) => { 
                let mut this = vec![car.clone()];
                let mut rest = cdr.collapse()?;
                this.append(&mut rest);
                Some(this)
            },
            _ => None
        }
    }

    // Pull an expr into a borrowed list
    pub fn collapse_b(&self) -> Option<Vec<&Rc<Expr>>> { 
        match self { 
            Nil => Some(vec![]),
            Cons(car,cdr) => { 
                let mut this = vec![car];
                let mut rest = cdr.collapse_b()?;
                this.append(&mut rest);
                Some(this)
            },
            _ => None
        }
    }

    // Compute the free symbols in an expression
    pub fn free_acc(&self, bound: &Vec<&String>) -> Vec<String> { 
        match &self { 
            // Symbols are free if they are not bound or keywords
            Symbol(s) => if bound.contains(&s) || is_keyword(&s) {
                            vec![] 
                         } else { 
                            vec![s.to_string()] 
                         },
            Cons(car,cdr) => { 
                let mut f_car = car.free_acc(bound);
                let mut f_cdr = cdr.free_acc(bound);
                f_car.append(&mut f_cdr);
                f_car
            },
            Closure(params, body, _) => {
                let mut now_bound:Vec<&String> = vec![];
                for i in params.iter() { 
                    now_bound.push(i);
                }
                for i in bound.iter() { 
                    now_bound.push(i);
                }
                body.free_acc(&now_bound)
            },
            _ => vec![],
        }

    }

    // Replace dummy bounds w/ recursive values
    pub fn replace(&self, name: &String, value: &Rc<Expr>) { 
        match self { 
            Cons (car,cdr) => { 
                car.replace(name, value);
                cdr.replace(name, value);
            },
            Closure(binds,body,env) => { 
                if !binds.contains(name) { 
                    body.replace(name, value);
                    env.replace(name, value)
                }
            },
            _ => ()
        }
    }

    fn print_cons(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result { 
        match self { 
            Cons (car,cdr) => match self.collapse() { 
                                    Some(lst) => { 
                                        let mut s = "'(".to_string();
                                        for e in lst.iter() {
                                            let t = format!("{} ", e);
                                            s.push_str(&t);
                                        }
                                        s.pop();
                                        s.push_str(")");
                                        write!(f, "{}", s)
                                    },
                                    None => write!(f, "({} {})", car, cdr),
                                },
            _ => panic!("Method should only have been passed cons!"),
        }
    }

}

impl fmt::Display for Expr { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self { 
            Int (i) => write!(f, "{}", i),
            Symbol (s) => write!(f, "'{}", s),
            Str (s) => write!(f, "{}", s),
            Cons (_,_) => self.print_cons(f),
            Nil => write!(f, "'()"), 
            Closure(_,_,_) => write!(f, "<closure>"),
            Builtin(_) => write!(f, "<Builtin>"),
            Dummy => write!(f, "<rec-dummy>"),
        }
    }
}

impl fmt::Debug for Expr { 
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self { 
            Int(i) => write!(f, "{}", i),
            Symbol(s) => write!(f, "{:?}", s),
            Str(s) => write!(f, "\"{}\"", s),
            Cons(_,_) => self.print_cons(f),
            Nil => write!(f, "'()"),
            Closure(_,_,_) => write!(f, "<closure>"),
            Builtin(_) => write!(f, "<builtin>"),
            Dummy => write!(f, "<rec-dummy"),
        }
    }
}

impl PartialEq for Expr { 
    // NOTE: Equality makes no sense for functions
    // So they are never equal
    fn eq(&self, rhs: &Expr) -> bool { 
        match (self, rhs) { 
            (Int(a), Int(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Cons(acar,acdr), Cons(bcar,bcdr)) => acar == bcar && acdr == bcdr,
            (Nil, Nil) => true,
            (Dummy, Dummy) => true,
            _ => false,
        }
    }
}

impl Env { 

    pub fn blank() -> Self { 
        Env { data: RefCell::new(HashMap::new()), next: None }
    }

    pub fn new(names: Vec<String>, vals: Vec<Rc<Expr>>, next: Option<Rc<Env>>) 
        -> Self 
    {
        let mut data = HashMap::new();
        for (name,val) in names.into_iter().zip(vals.iter()) { 
            data.insert(name, RefCell::new(val.clone()));
        }
        Env { data: RefCell::new(data), next } 
    }

    pub fn lookup(&self, name: &str) -> Option<Rc<Expr>> { 
        match self.data.borrow().get(name) { 
            Some(r) => Some(r.borrow().clone()),
            None => self.next.as_ref()?.lookup(name),
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }

    pub fn set(&self, name: String, val: Rc<Expr>) {
        self.data.borrow_mut().insert(name, RefCell::new(val));
    }

    // Replace dummy values w/ recursive bounds
    pub fn replace(&self, name: &String, value: &Rc<Expr>) {
        match self.data.borrow().get(name) { 
            Some(expr_ptr) => { 
                let expr = expr_ptr.borrow();
                if **expr == Dummy { 
                    // Releaes the shared reference expr so we 
                    // can acquire a mutable reference
                    drop(expr); 

                    // Load the value into a RefCell and then swap it
                    let new = RefCell::new(value.clone());
                    expr_ptr.swap(&new);
                }
            },
            _ => ()
        }
    }

    pub fn get_toplevel<'a>(env: &'a Rc<Env>) -> &'a Rc<Env> { 
        match &env.next { 
            Some(e) => Env::get_toplevel(e),
            None => env
        }
    }


}


impl fmt::Display for EvalError { 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        match self { 
            ArityError (got,exp) => write!(f, "Arity Error!, expected {}, got {}", exp, got),
            LetError => write!(f, "Syntax Error in Let Form"),
            IfError => write!(f, "Syntax Error in If Form"),
            DefineError => write!(f, "Syntax Error in Define Form"),
            ClosureError => write!(f, "Syntax Error in Closure Form"),
            ApplicationNonFunction => write!(f, "Application of non-function"),
            UnboundSymbol (s) => write!(f, "Unbound symbol {}", s),
            TypeError(src,want,got) => write!(f, "Type Error in {}, wanted {}, got {}", src, want, got),
            Generic(s) => write!(f, "{}", s),
            DummyError => write!(f, "A dummy value was evaluated!"),
            UnQuoteOutsideQuote => write!(f, "unquote outside of a quasiquote form!"),
        }
            
    }

}

fn is_keyword(s: &String) -> bool {
    for kw in KEYWORDS.iter() { 
        if kw == s { return true; }
    }
    false
}
