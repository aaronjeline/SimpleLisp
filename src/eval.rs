// Core evaluation functions

use crate::expr::{*, Expr::*, EvalError::*};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;


// Toplevel, evaluate an expression in a given environment
pub fn eval(e: Rc<Expr>, env: &Rc<Env>) -> EResult { 
    match &*e { 
        Int(_) => Ok(e),
        Symbol(s) => env.lookup(s).ok_or(UnboundSymbol(s.to_string())),
        Str (s) => Ok(e),
        Nil => Ok(e),
        Closure(_,_,_) => Ok(e),
        Builtin(_) => Ok(e),
        Cons(car,cdr) => { 
            match e.collapse() { 
                Some(lst) => eval_list(lst, env),
                None => Ok(e),
            }
        }
        _ => Err(DummyError),

    }
}

// Evaluate list, either a builtin form or a function call
fn eval_list(lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    match &*lst[0] { 
        Symbol(s) => 
            match s.as_ref() {
                "if" => eval_if(lst, env),
                "let" => eval_let(lst, env),
                "fun" => eval_closure(lst, env),
                "quote" => eval_quote(lst, env),
                "define" => eval_define(lst, env),
                "begin" => eval_begin(lst, env),
                "quasiquote" => eval_quasiquote(lst, env),
                "unquote" => Err(UnQuoteOutsideQuote),
                "catch" => eval_catch(lst, env),
                _ => eval_call(lst, env),
            },
        _ => eval_call(lst, env)
        
    }
}

fn eval_catch(mut lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    // (catch body error finally)
    if lst.len() == 4 {
        let finally = lst.pop().unwrap();
        let error = lst.pop().unwrap();
        let error = match &*error {
            Str(s) => Ok(s),
            e => Err(TypeError("catch", "string", e.get_type())),
        }?;
        let body = lst.pop().unwrap();
        match eval(body, env) { 
            Err(Generic(s)) if &s == error => eval(finally, env),
            e => e,
        }
    } else { 
        Err(ArityError(4, lst.len()))
    }

}

fn eval_begin(mut lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    lst.remove(0);
    let mut result:Option<Rc<Expr>> = None;
    for e in lst.into_iter() { 
        result = Some(eval(e, env)?);
    }

    Ok(result.unwrap_or(Rc::new(Nil)))
}

fn eval_define(lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    // (define name value)
    if lst.len() != 3 { 
        Err(ArityError(3, lst.len()))
    } else { 
        let name = get_symbol(&lst[1].clone()).ok_or(DefineError)?.to_string();
        let value = lst[2].clone();
        // Build a let expr
        let binding = Cons(Rc::new(Symbol(name.clone())),
                           Rc::new(Cons(value,
                                   Rc::new(Nil))));
        let letexpr = Cons(Rc::new(Symbol("let".to_string())),
                           Rc::new(Cons(Rc::new(binding),
                                        Rc::new(Cons(Rc::new(Symbol(name.clone())),
                                                     Rc::new(Nil))))));

        let value = eval(Rc::new(letexpr), env)?;
        let tl = Env::get_toplevel(env);
        tl.set(name, value); 

        Ok(Rc::new(Nil))
    }
}


fn eval_quote(lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    if lst.len() == 2 { 
        Ok(lst[1].clone())
    } else { 
        Err(ArityError(2, lst.len()))
    }
}

fn eval_quasiquote(mut lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    if lst.len() == 2 { 
        let e = lst.pop().unwrap();
        quasiquote_traverse(e, env)
    } else { 
        Err(ArityError(2, lst.len()))
    }
}

fn quasiquote_traverse(e: Rc<Expr>, env: &Rc<Env>) -> EResult { 
    match &*e { 
        Cons(car,cdr) if is_unquote(&e) => { 
            let mut parts = e.collapse().unwrap();
            eval(parts.pop().unwrap(), env)
        }
        Cons(car,cdr) => Ok(Rc::new(Cons(quasiquote_traverse(car.clone(), env)?,
                                         quasiquote_traverse(cdr.clone(), env)?))),
        _ => Ok(e),
    }
}

fn is_unquote(e: &Rc<Expr>) -> bool { 
    match &**e {
        Cons(car, cdr) => match e.collapse() { 
            Some(lst) => lst.len() == 2 && *lst[0] == Symbol("unquote".to_string()),
            None => false,
        },
        _ => false,
    }
}


// Construct a closure
fn eval_closure(lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    // (fun (x y ...) body)
    if lst.len() != 3 { 
        Err(ArityError(3, lst.len()))
    } else { 
        let body = lst[2].clone();
        let args:Vec<&String> = gather_args(&lst[1])?;
        // Compute free symbols in the closure body
        // These need to be captured and stored in the
        // Closure environment
        let free:Vec<String> = body.free_acc(&args);
        let values:Vec<Rc<Expr>> = accumulate_values(&free, env)?;
        let captured = Env::new(free, values, None);
        let args:Vec<String> = args
                                .into_iter()
                                .map(|s| s.to_string())
                                .collect();

        Ok(Rc::new(Closure(args, body, Rc::new(captured))))
    }
}

//Lookup all values in the current env
fn accumulate_values(free: &Vec<String>, env: &Rc<Env>) -> Result<Vec<Rc<Expr>>> {
    let mut values = vec![];
    for name in free { 
        match env.lookup(name) { 
            Some(e) => values.push(e),
            None => return Err(UnboundSymbol(name.clone()))
        }
    }

    Ok(values)
}

// Parse argument lists
fn gather_args(args: &Rc<Expr>) -> Result<Vec<&String>> {
    let lst:Vec<&Rc<Expr>> = args.collapse_b().ok_or(ClosureError)?;
    let args:Option<Vec<&String>> = lst.into_iter().map(get_symbol).collect();
    args.ok_or(ClosureError)
}

fn get_symbol(e: &Rc<Expr>) -> Option<&String> { 
    match &**e {
        Symbol(s) => Some(&s),
        _ => None,
    }
}


// Evaluate the `if` form
fn eval_if(lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    // (if guard if-true if-false)

    if lst.len() != 4 { 
        Err(ArityError(4, lst.len()))
    } else { 
        let guard = eval(lst[1].clone(), env)?;
        let branch = match &*guard {
            Nil => lst[3].clone(),
            _ => lst[2].clone(),
        };
        eval(branch, env)
    } 
}   

// Evaluate the `let` form
fn eval_let(lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    // (let (symbol value) body) 
    if lst.len() != 3 { 
        Err(ArityError(3, lst.len()))
    } else { 
        let body = lst[2].clone();
        let (name, value) = eval_binding(lst[1].clone(), env)?;
        // Create the new environment
        let new = Rc::new(Env::new(vec![name], vec![value], Some(env.clone())));
        eval(body, &new)
    }

}

// Evaluate a potentially recursive binding
fn eval_binding(e: Rc<Expr>, env: &Rc<Env>) -> Result<(String, Rc<Expr>)> { 
    // (name value)
    let lst:Vec<_> = e.collapse().ok_or(LetError)?;
    if lst.len() != 2 { 
        Err(LetError)
    } else { 
        let name = match &*lst[0] { 
            Symbol(s) => Ok(s.to_string()),
            _ => Err(LetError),
        }?;
        // Create a temporary environment that binds the new name
        let tempenv = 
            Rc::new(Env::new(vec![name.clone()], 
                             vec![Rc::new(Dummy)], Some(env.clone())));
        // Evaluate under the temp env
        let value = eval(lst[1].clone(), &tempenv)?;
        // Replace the dummy binding w/ the new value
        value.replace(&name, &value);
        Ok((name, value))
    }

}

// Evaluate calls
fn eval_call(lst: Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    type T = Result<Vec<Rc<Expr>>>;
    // Evaluate all subterms
    let mut evald:Vec<Rc<Expr>> = lst.into_iter()
                                    .map(|e| eval(e, env))
                                    .collect::<T>()?;
    if evald.len() == 0 { //Empty calls don't make sense
        Err(ApplicationNonFunction)
    } else { 
        let f = evald.remove(0); // Get the function
        match &*f { 
            Closure(_,_,_) => apply(f, evald, env),
            Builtin(p) => apply(f, evald, env),
            _ => Err(ApplicationNonFunction)
        }
    }
}

// Apply fucntions
pub fn apply(f: Rc<Expr>, args:Vec<Rc<Expr>>, env: &Rc<Env>) -> EResult { 
    match &*f { 
        Closure(params, body, cap) => { 
            // Thread capture through current env
            let new = Rc::new(Env::new(params.to_vec(), args, Some(cap.clone())));
            eval(body.clone(), &new)    
        },
        Builtin(p) => p(args),
        _ => panic!()
    }

}
