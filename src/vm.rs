use crate::object::{Object, Integer};
use crate::op_code::{Instructions, OpCode};
use crate::compiler::ByteCode;
use crate::utils::from_u16;

pub type VMError = String;
pub type VMResult = std::result::Result<(), VMError>;


const STACK_SIZE : usize = 2048;

pub struct VM {
    constants: Vec<Box<dyn Object>>,
    instructions: Instructions,

    stack: Vec<Box<dyn Object>>,
    sp: usize,
}

impl VM {
    pub fn new(bytecode: ByteCode) -> Self {
        Self{
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
        }
    }

    pub fn run(&mut self) -> VMResult {
        let mut ip = 0;
        loop {
            if ip >= self.instructions.len() {
                break;
            }

            let op_code = OpCode::from_byte(self.instructions[ip]);

            match op_code {
                OpCode::OpConstant => {
                    let index = from_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.push(self.constants[index as usize].clone())?;
                },
                OpCode::OpAdd => {
                    let right = self.pop();
                    let left = self.pop();

                    let right = right.as_any().downcast_ref::<Integer>().unwrap();
                    let left = left.as_any().downcast_ref::<Integer>().unwrap();
                    let result = left.value + right.value;
                    self.push(Box::new(Integer{value: result}))?;
                },
                _ => return Err(format!("Unhandled op code: {:?}", op_code))
            }

            ip += 1;
        }

        Ok(())
    }

    fn push(&mut self, object: Box<dyn Object>) -> VMResult {
        if self.sp >= STACK_SIZE {
            return Err("Stack overflow".to_string());
        }
        self.stack.insert(self.sp, object);
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Box<dyn Object> {
        let obj = self.stack.pop().unwrap_or_else(|| panic!("Cannot pop from an empty stack"));
        self.sp -= 1;

        obj
    }

    pub fn top(&self) -> Option<Box<dyn Object>> {
        if self.sp == 0 {
            return None;
        }

        Some(self.stack[self.sp - 1].clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::object::Integer;
    use crate::parser::Parser;

    use super::*;
    // use crate::code::OpCodes;
    // use crate::disassembler::Disassembler;

    struct TestCase<'a> {
        input: &'a str,
        expected: Box<dyn Object>,
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            TestCase { input: "1", expected: Box::new(Integer { value: 1 }) },
            TestCase { input: "2", expected: Box::new(Integer { value: 2 }) },
            TestCase { input: "1 + 2", expected: Box::new(Integer { value: 3 }) },
        ];

        run_tests(tests)
    }

    fn run_tests(tests: Vec<TestCase>) {
        for t in tests {
            let program = parse(t.input);
            let mut compiler = Compiler::new();
            let error = compiler.compile(program);
            assert!(error.is_ok());

            let bytecode = compiler.bytecode();
            // let disassembler = Disassembler::new(OpCodes::new());
            // println!("{}", disassembler.disassemble(bytecode.instructions.clone()));

            // for (i, c) in bytecode.constants.iter().enumerate() {
            //     println!("#{}: {}", i, c.inspect());
            // }
            let mut vm = VM::new(bytecode);
            let res = vm.run();
            if res.is_err() {
                println!("{:?}", res);
            }
            assert!(res.is_ok());

            let top = vm.top().unwrap();

            assert_eq!(top.inspect(), t.expected.inspect());
        }
    }

    fn parse(input: &str) -> Box<dyn Node> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap_or_else(|| panic!("Invalid program"))
    }
}