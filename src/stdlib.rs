// Contains standard definitions for builtin procs

use crate::{expr, expr::{*, Expr::*, EvalError::*}};
use crate::parser::parse;
use crate::eval;
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
use num_bigint::BigInt;
use num_traits::{Zero, One};
use std::fs::read_to_string;

const STDLIB: &'static str = "./stdlib.lsp";

pub fn build_std() -> Rc<Env> { 
    let mut e = Env::blank();
    e.set("+".to_string(), Rc::new(Builtin(plus)));
    e.set("*".to_string(), Rc::new(Builtin(mult)));
    e.set("zero?".to_string(), Rc::new(Builtin(zero)));
    e.set("sub1".to_string(), Rc::new(Builtin(sub1)));
    e.set("cons".to_string(), Rc::new(Builtin(cons)));
    e.set("cdr".to_string(), Rc::new(Builtin(cdr)));
    e.set("car".to_string(), Rc::new(Builtin(car)));
    e.set("dump".to_string(), Rc::new(Builtin(dump)));
    e.set("read".to_string(), Rc::new(Builtin(read)));
    e.set("slurp".to_string(), Rc::new(Builtin(slurp)));
    e.set("apply".to_string(), Rc::new(Builtin(apply)));
    e.set("write".to_string(), Rc::new(Builtin(write)));
    e.set("raise".to_string(), Rc::new(Builtin(raise)));


    let e = Rc::new(e);

    load_stdlib(&e);

    e
}

fn load_stdlib(env: &Rc<Env>) { 
    match read_to_string(STDLIB) { 
        Ok(s) => 
            match parse(&s) { 
                Some(e) => match eval(Rc::new(e), env) { 
                    Ok(_) => (),
                    Err(e) => panic!(format!("Error executing stdlib: {}", e)),
                },
                None => panic!("Syntax error in standard lib"),
            },
        Err(_) => panic!("Error openning stdlib!"),
    }
}

fn generic(s: &str) -> EvalError { 
    Generic(s.to_string())
}

fn raise(mut e: Vec<Rc<Expr>>) -> EResult { 
    let expr = e.pop().ok_or(ArityError(1, e.len()))?;
    match &*expr { 
        Str(s) => Err(generic(s)),
        _ => Err(TypeError("raise", "string", expr.get_type()))

    }
}

fn write(mut e: Vec<Rc<Expr>>) -> EResult { 
    if e.len() != 2 { 
        Err(ArityError(2, e.len()))
    } else {
        let data = e.pop().unwrap();
        let fname = e.pop().unwrap();
        match &*fname { 
            Str(s) => {
                let mut file = File::create(s).or(Err(generic("File Open Error")))?;
                let bytes:Vec<_> = format!("{:?}", data).bytes().collect();
                file.write(&bytes[..]).or(Err(generic("File Write Error")))?;
                Ok(Rc::new(Nil))
            },
            _ => Err(TypeError("write", "string", fname.get_type()))
        }
    }
}

fn apply(mut e: Vec<Rc<Expr>>) -> EResult { 
    let env = Rc::new(Env::blank());
    if e.len() != 2 { 
        Err(ArityError(2, e.len()))
    } else { 
        let f = e[0].clone();
        let args = e[1].clone();
        let fIsFunction = match &*f { 
            Builtin(_) => true,
            Closure(_,_,_) => true,
            _ => false,
        };

        match (fIsFunction, args.is_list()) { 
            (true,true) => eval::apply(f, args.collapse().unwrap(), &env),
            (false,_) => Err(TypeError("apply", "<function>", f.get_type())),
            _ => Err(TypeError("apply", "list", f.get_type())),
        }


    }

}

fn slurp(e: Vec<Rc<Expr>>) -> EResult { 
    match e.get(0) { 
        Some(e) => match &**e { 
                Str(s) => read_to_string(s).map_or(Err(generic("File Read Error")), |s| Ok(Rc::new(Str(s)))),
                _ => Err(TypeError("slurp", "string", e.get_type())),
            },
        None => Err(ArityError(1, 0)),
    }
}
            

fn read(mut e: Vec<Rc<Expr>>) -> EResult { 
    match e.get(0) { 
        Some(e) => match &**e {
            Str(s) => parse(s).map(|x| Rc::new(x)).ok_or(generic("Read Error")),
            _ => Err(TypeError("read", "string", e.get_type())),
        },
        None => Err(ArityError(1, 0))
    }

                
            
}

fn dump(mut e: Vec<Rc<Expr>>) -> EResult { 
    match e.pop() { 
        Some(e) => Ok(Rc::new(Str(format!("{:?}", e)))),
        None => Err(ArityError(1, 0))
    }
}


fn cons(e: Vec<Rc<Expr>>) -> EResult { 
    if e.len() == 2 { 
        Ok(Rc::new(Cons(e[0].clone(), e[1].clone())))    
    } else { 
        Err(ArityError(2, e.len()))
    }
}

fn car(e: Vec<Rc<Expr>>) -> EResult { 
    match &**e.get(0).ok_or(ArityError(1, 0))? { 
        Cons(car,_) => Ok(car.clone()),
        e => Err(TypeError("car", "pair", e.get_type())),
    }
}

fn cdr(e: Vec<Rc<Expr>>) -> EResult { 
    match &**e.get(0).ok_or(ArityError(1, 0))? { 
        Cons(_,cdr) => Ok(cdr.clone()),
        e => Err(TypeError("cdr", "pair", e.get_type())),
    }
}

    
fn sub1(e: Vec<Rc<Expr>>) -> EResult { 
    let o:BigInt = One::one();
    if e.len() == 1 { 
        match &*e[0] { 
            Int(i) => Ok(Rc::new(Int(i - &o))),
            e => Err(TypeError("sub1", "int", e.get_type())),
        }
    } else { 
        Err(ArityError(1, e.len()))
    }
}


fn zero(e: Vec<Rc<Expr>>) -> EResult { 
    let z:BigInt = Zero::zero();
    if e.len() == 1 { 
        match &*e[0] { 
            Int(i) => Ok(Rc::new(if i == &z { Int(One::one()) } else { Nil } )),
            e => Err(TypeError("zero?", "int", e.get_type())),
        }
    } else { 
        Err(ArityError(1, e.len()))
    }
}

fn plus(e: Vec<Rc<Expr>>) -> EResult { 
    let mut sum:BigInt = Zero::zero();

    for x in e.into_iter() { 
        match &*x  {
            Int(i) => sum += i,
            e => return Err(TypeError("+", "int", e.get_type())),
        }

    }

    Ok(Rc::new(Int(sum)))
}

fn mult(e: Vec<Rc<Expr>>) -> EResult { 
    let mut sum:BigInt = One::one();

    for x in e.into_iter() { 
        match &*x  {
            Int(i) => sum *= i,
            e => return Err(TypeError("*", "int", e.get_type())),
        }

    }

    Ok(Rc::new(Int(sum)))

}
