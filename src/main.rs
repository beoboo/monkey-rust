use std::io;

use users::get_current_username;

use crate::repl::Repl;

mod token;
mod lexer;
mod repl;
mod parser;
mod ast;
mod object;
mod evaluator;
mod environment;
mod builtins;
mod macro_expander;
mod utils;
mod modifier;
mod op_code;
mod compiler;
mod assembler;
mod disassembler;
mod vm;
mod symbol_table;

fn main() {
    let username = get_current_username().unwrap();

    println!("Hello, {}! This is the Monkey programming language!", username.to_string_lossy());
    println!("Feel free to type in commands");

    Repl::start(io::stdin(), io::stdout());
}

