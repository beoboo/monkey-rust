use crate::op_code::{Instructions, OpCode, OpCodes};
use crate::object::Object;
use crate::ast::{Node, Program};
use crate::assembler::Assembler;

pub type CompilerError = String;
pub type CompilerResult = std::result::Result<(), CompilerError>;

pub struct ByteCode {
    pub instructions: Instructions,
    pub constants: Vec<Box<dyn Object>>,
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Box<dyn Object>>,
    assembler: Assembler,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: vec![],
            assembler: Assembler::new(OpCodes::new()),
        }
    }

    pub fn compile(&mut self, node: Box<dyn Node>) -> CompilerResult {
        node.compile(self)
    }

    pub fn compile_program(&mut self, program: &Program) -> CompilerResult {
        for stmt in &program.statements {
            match self.compile(stmt.clone_node()) {
                Ok(_) => {}
                Err(error) => return Err(error)
            }
        }

        Ok(())
    }

    pub fn bytecode(&self) -> ByteCode {
        ByteCode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone()
        }
    }

    pub fn emit(&mut self, op_code: OpCode, operands: Vec<u32>) -> u32 {
        let instruction = self.assembler.assemble(op_code, operands);
        self.add_instructions(instruction)
    }

    pub fn add_instructions(&mut self, instruction: Instructions) -> u32 {
        self.instructions.extend(instruction);

        (self.instructions.len() - 1) as u32
    }

    pub fn add_constant(&mut self, constant: Box<dyn Object>) -> u32 {
        self.constants.push(constant);

        (self.constants.len() - 1) as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::{Object, Integer};
    use crate::assembler::Assembler;
    use crate::op_code::OpCode;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    struct TestCase<'a> {
        input: &'a str,
        expected_instructions: Vec<Instructions>,
        expected_constants: Vec<Box<dyn Object>>
    }

    #[test]
    fn test_integer_arithmetic() {
        let assembler = Assembler::new(OpCodes::new());
        let tests = vec![
            TestCase{
                input: "1 + 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpAdd, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            }
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

            test_instructions(bytecode.instructions, t.expected_instructions);
            test_constants(bytecode.constants, t.expected_constants);
        }
    }

   fn parse(input: &str) -> Box<dyn Node> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap_or_else(|| panic!("Invalid program"))
    }

    fn test_instructions(actual: Instructions, expected: Vec<Instructions>) {
        let expected = concat_instructions(expected);
        assert_eq!(actual.len(), expected.len());

        for (i, ins) in expected.iter().enumerate() {
            assert_eq!(actual[i], *ins);
        }
    }

    fn concat_instructions(instructions: Vec<Instructions>) -> Instructions {
        instructions.concat()
    }

    fn test_constants(actual: Vec<Box<dyn Object>>, expected: Vec<Box<dyn Object>>) {
        assert_eq!(actual.len(), expected.len());

        for (i, ins) in expected.iter().enumerate() {
            // println!("{:?}", actual[i].inspect());
            // println!("{:?}", ins.inspect());
            assert_eq!(actual[i].inspect(), ins.inspect());
        }
    }
}
