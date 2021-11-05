use std::io::{Stdin, Stdout, Write};

use clap::{App, Arg};

use crate::compiler::Compiler;
use crate::environment::Environment;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::macro_expander::MacroExpander;
use crate::parser::Parser;
use crate::vm::VM;

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
        let mut environment = Environment::new();
        let mut macro_environment = Environment::new();
        let matches = App::new("Monkey")
            .arg(Arg::with_name("enable-vm")
                .long("enable-vm")
                .help("Enables the virtual machine")
            ).get_matches();

        let enable_vm = matches.is_present("enable-vm");
        if enable_vm {
            println!("Using the virtual machine")
        }

        loop {
            output.write(PROMPT.as_bytes()).ok();
            output.flush().ok();

            let mut line = String::new();
            input.read_line(&mut line).map_err(|_| { return; }).ok();

            let lexer = Lexer::new(line.as_str());
            let mut parser = Parser::new(lexer);

            let mut program = match parser.parse_program() {
                Some(program) => program,
                None => { continue; }
            };

            if parser.errors.len() > 0 {
                print_errors(parser.errors);
                continue;
            }

            let expander = MacroExpander::new();
            expander.define_macros(&mut program, &mut macro_environment);
            let expanded = expander.expand_macros(&program, &mut macro_environment);

            if enable_vm {
                let mut compiler = Compiler::new();
                match compiler.compile(program) {
                    Ok(_) => {}
                    Err(err) => {
                        print_errors(vec![err]);
                        continue;
                    }
                }

                let mut vm = VM::new(compiler.bytecode());
                match vm.run() {
                    Ok(_) => {}
                    Err(err) => {
                        print_errors(vec![err]);
                        continue;
                    }
                }

                let top = vm.last_top.unwrap();
                println!("{}", top.inspect())
            } else {
                let evaluator = Evaluator::new();

                match evaluator.eval(Box::new(expanded.as_node()), &mut environment) {
                    Some(evaluated) => println!("{}", evaluated.inspect()),
                    None => {}
                }
            }
        }
    }
}

fn print_errors(errors: Vec<String>) {
    println!("{}", MONKEY_FACE);
    println!("Woops! We ran into some monkey business here");
    println!(" parser errors: ");

    for error in errors {
        println!("\t{}", error);
    }
}
