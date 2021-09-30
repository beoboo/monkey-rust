use std::io::{Stdin, Stdout, Write};
use crate::lexer::Lexer;
use crate::token::TokenType;

pub struct Repl {}

const PROMPT: &str = ">> ";

impl Repl {
    pub fn start(input: Stdin, mut output: Stdout) {
        loop {
            output.write(PROMPT.as_bytes()).ok();
            output.flush().ok();

            let mut line = String::new();
            input.read_line(&mut line).map_err(|_| { return}).ok();

            let mut lexer = Lexer::new(line.as_str());

            loop {
                let token = lexer.next_token();

                if token.token_type == TokenType::EOF {
                    break;
                }

                println!("{:?}", token);
            }
        }
    }
}