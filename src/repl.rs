use std::io::{Stdin, Stdout, Write};

use crate::ast::Node;
use crate::environment::Environment;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub struct Repl {}

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = "            __,__
   .--.  .-\"     \"-.  .--.
  / .. \\/  .-. .-.  \\/ .. \\
 | |  '|  /   Y   \\  |'  | |
 | \\   \\  \\ 0 | 0 /  /   / |
  \\ '- ,\\.-\"\"\"\"\"\"\"-./, -' /
   ''-' /_   ^ ^   _\\ '-''
       |  \\._   _./  |
       \\   \\ '~' /   /
        '._ '-=-' _.'
           '-----'
";


impl Repl {
    pub fn start(input: Stdin, mut output: Stdout) {
        loop {
            output.write(PROMPT.as_bytes()).ok();
            output.flush().ok();

            let mut line = String::new();
            input.read_line(&mut line).map_err(|_| { return; }).ok();

            let lexer = Lexer::new(line.as_str());
            let mut parser = Parser::new(lexer);

            let program = match parser.parse_program() {
                Some(program) => program,
                None => { continue; }
            };

            if parser.errors.len() > 0 {
                Repl::print_parser_errors(parser.errors);
                continue;
            }

            let evaluator = Evaluator::new();
            let mut environment = Environment::new();
            match evaluator.eval(Box::new(program.as_node()), &mut environment) {
                Some(evaluated) => println!("{}", evaluated.inspect()),
                None => {}
            }
        }
    }

    fn print_parser_errors(errors: Vec<String>) {
        println!("{}", MONKEY_FACE);
        println!("Woops! We ran into some monkey business here");
        println!(" parser errors: ");

        for error in errors {
            println!("\t{}", error);
        }
    }
}