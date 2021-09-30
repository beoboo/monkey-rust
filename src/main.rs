use std::io;

use users::get_current_username;

use crate::repl::Repl;

mod token;
mod lexer;
mod repl;

fn main() {
    let username = get_current_username().unwrap();

    println!("Hello, {}! This is the Monkey programming language!", username.to_string_lossy());
    println!("Feel free to type in commands");

    Repl::start(io::stdin(), io::stdout());
}

