mod lex;
mod expr;
mod eval;
mod parser;
mod stdlib;

use expr::{*, Expr::*};
use eval::*;
use stdlib::*;

use std::rc::Rc;
use std::io::{BufRead, self};
use std::thread;
use std::sync::{Arc, Mutex};

type Flag = Arc<Mutex<bool>>;


fn main() {
    // Create two copies of flag, one will go to the 
    // thread that responds to SIGINT
    let main_flag:Flag = Arc::new(Mutex::new(false));
    // This flag will be moved to handler thread 
    // as it is captured by the closure
    let handler_flag = main_flag.clone(); 
    // Create the SIGINT handler
    ctrlc::set_handler(move || { 
        let mut flag = handler_flag.lock().unwrap();
        *flag = true;
        println!("");
    }).expect("Error setting C-c handler");
    
    // Spawn the repl, giving it the other arc holding the flag
    repl(main_flag);
}

// Main controlling function
fn repl(flag:Flag) { 
    let stdin = io::stdin();
    let toplevel = build_std(); // Top level enviornment

    let mut src = "".to_string(); // Keep track of the currently built source

    for line in stdin.lock().lines() {  // Read lines from stdin
        // Open the mutex to access the clear flag
        let mut f = flag.lock().unwrap();
        if *f { 
            // Clear the accumated src string
            src = "".to_string();
            *f = false;
        }
        drop(f); // Release the mutex
    

        let mut line = line.unwrap();
        line.push_str(" ");
        src.push_str(&line);
        // We only want to run the string if it's balanced
        // Otherwise we let the user keep building it
        if is_closed(&src) {
            run(&src, &toplevel);
            src = "".to_string();
        }
    }
}

// Parse and evaluate a source string
fn run(src: &str, toplevel: &Rc<Env>) {
    match parser::parse(&src) { 
        Some(e) => match eval(Rc::new(e), toplevel) { 
            Ok(e) => println!("{}", e),
            Err(e) => println!("Runtime Error: {}", e),
        },
        None => println!("Syntax Error!"),
    }
}

// Check if a string has balanced parens
// NOTE: This is _not_ a full parse
fn is_closed(s: &str) -> bool { 
    let mut p = 0;
    for c in s.chars() { 
        match c {
            '(' => p += 1,
            ')' => p -= 1,
            _ => (),
        }
    }

    p == 0 
}





