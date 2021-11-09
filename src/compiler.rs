use crate::op_code::{Instructions, OpCode, OpCodes, Byte};
use crate::object::Object;
use crate::ast::{Node, Statement, LetStatement, Identifier};
use crate::assembler::Assembler;
use num_traits::FromPrimitive;
use crate::symbol_table::SymbolTable;

pub type CompilerError = String;
pub type CompilerResult = std::result::Result<(), CompilerError>;

pub struct ByteCode {
    pub instructions: Instructions,
    pub constants: Vec<Box<dyn Object>>,
}

#[derive(Clone, PartialEq)]
pub struct EmittedInstruction {
    op_code: OpCode,
    position: usize,
}

pub struct Compiler<'a> {
    pub instructions: Instructions,
    assembler: Assembler,
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
    constants: &'a mut Vec<Box<dyn Object>>,
    symbol_table: &'a mut SymbolTable,
}

impl<'a> Compiler<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable, constants: &'a mut Vec<Box<dyn Object>>) -> Self {
        Self {
            instructions: Instructions::new(),
            assembler: Assembler::new(OpCodes::new()),
            last_instruction: EmittedInstruction{op_code: OpCode::Invalid, position: 0},
            previous_instruction: EmittedInstruction{op_code: OpCode::Invalid, position: 0},
            constants,
            symbol_table,
        }
    }

    pub fn compile(&mut self, node: Box<dyn Node>) -> CompilerResult {
        node.compile(self)
    }

    pub fn compile_statements(&mut self, statements: &Vec<Box<dyn Statement>>) -> CompilerResult {
        for stmt in statements {
            match self.compile(stmt.clone_node()) {
                Ok(_) => {}
                Err(error) => return Err(error)
            }
        }

        Ok(())
    }

    pub fn compile_let_statement(&mut self, stmt: &LetStatement) -> CompilerResult {
        self.compile(stmt.value.clone_node())?;

        let symbol = self.symbol_table.define(stmt.clone().name.value);
        self.emit(OpCode::OpSetGlobal, vec![symbol.index as u32]);

        Ok(())
    }

    pub fn compile_identifier(&mut self, identifier: &Identifier) -> CompilerResult {
        match self.symbol_table.resolve(&identifier.value) {
            Some(symbol) => {
                let index = symbol.index as u32;
                self.emit(OpCode::OpGetGlobal, vec![index]);
            },
            None => return Err(CompilerError::from(format!("Undefined variable {}", identifier.value)))
        };

        Ok(())
    }

    pub fn bytecode(&self) -> ByteCode {
        ByteCode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone()
        }
    }

    pub fn emit(&mut self, op_code: OpCode, operands: Vec<u32>) -> usize {
        let instruction = self.assembler.assemble(op_code, operands);
        let pos = self.add_instructions(instruction);
        self.set_last_instruction(op_code, pos);

        pos
    }

    fn set_last_instruction(&mut self, op_code: OpCode, position: usize) {
        let previous = self.last_instruction.clone();
        let last = EmittedInstruction{op_code, position};
        self.previous_instruction = previous;
        self.last_instruction = last;
    }

    pub fn add_instructions(&mut self, instruction: Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend(instruction);

        pos
    }

    pub fn add_constant(&mut self, constant: Box<dyn Object>) -> u32 {
        self.constants.push(constant);

        (self.constants.len() - 1) as u32
    }

    pub fn pop_last_pop_instruction(&mut self) {
        if self.last_instruction.op_code == OpCode::OpPop {
            self.instructions.pop();
            self.last_instruction = self.previous_instruction.clone();
        }
    }

    pub fn replace_instruction(&mut self, position: usize, bytes: Vec<Byte>) {
        for (i, b) in bytes.iter().enumerate() {
            self.instructions[position + i] = *b;
        }
    }

    pub fn change_operand(&mut self, position: usize, operand: u32) {
        let op_code = FromPrimitive::from_u8(self.instructions[position]).unwrap_or_else(|| panic!("Cannot convert op code: {}", self.instructions[position]));
        let instruction = self.assembler.assemble(op_code, vec![operand]);
        self.replace_instruction(position, instruction);
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
    use crate::disassembler::Disassembler;

    struct TestCase<'a> {
        input: &'a str,
        expected_instructions: Vec<Instructions>,
        expected_constants: Vec<Box<dyn Object>>
    }

    #[test]
    fn test_integers() {
        let assembler = Assembler::new(OpCodes::new());
        let tests = vec![
            TestCase{
                input: "1 + 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpAdd, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            },
            TestCase{
                input: "1; 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            },
            TestCase{
                input: "1 - 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpSub, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            },
            TestCase{
                input: "1 * 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpMul, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            },
            TestCase{
                input: "1 / 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpDiv, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            },
            TestCase{
                input: "-1",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpMinus, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1})],
            },
        ];

        run_tests(tests)
    }

    #[test]
    fn test_booleans() {
        let assembler = Assembler::new(OpCodes::new());
        let tests = vec![
            TestCase{
                input: "true",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpTrue, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![],
            },
            TestCase{
                input: "false",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpFalse, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![],
            },
            TestCase{
                input: "1 > 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpGreaterThan, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            },
            TestCase{
                input: "1 < 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpGreaterThan, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 2}), Box::new(Integer{value: 1})],
            },
            TestCase{
                input: "1 == 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpEqual, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            },
            TestCase{
                input: "1 != 2",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpNotEqual, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![Box::new(Integer{value: 1}), Box::new(Integer{value: 2})],
            },
            TestCase{
                input: "true == false",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpTrue, vec![]),
                    assembler.assemble(OpCode::OpFalse, vec![]),
                    assembler.assemble(OpCode::OpEqual, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![],
            },
            TestCase{
                input: "true != false",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpTrue, vec![]),
                    assembler.assemble(OpCode::OpFalse, vec![]),
                    assembler.assemble(OpCode::OpNotEqual, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![],
            },
            TestCase{
                input: "!true",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpTrue, vec![]),
                    assembler.assemble(OpCode::OpBang, vec![]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![],
            },
        ];

        run_tests(tests)
    }

    #[test]
    fn test_conditionals() {
        let assembler = Assembler::new(OpCodes::new());
        let tests = vec![
            TestCase{
                input: "if (true) { 10 }; 3333;",
                expected_instructions: vec![
                    // 0000
                    assembler.assemble(OpCode::OpTrue, vec![]),
                    // 0001
                    assembler.assemble(OpCode::OpJumpNotTruthy, vec![10]),
                    // 0004
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    // 0007
                    assembler.assemble(OpCode::OpJump, vec![11]),
                    // 0010
                    assembler.assemble(OpCode::OpNull, vec![]),
                    // 0011
                    assembler.assemble(OpCode::OpPop, vec![]),
                    // 0012
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    // 0015
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![
                    Box::new(Integer{value: 10}),
                    Box::new(Integer{value: 3333}),
                ],
            },
            TestCase{
                input: "if (true) { 10 } else { 20 }; 3333;",
                expected_instructions: vec![
                    // 0000
                    assembler.assemble(OpCode::OpTrue, vec![]),
                    // 0001
                    assembler.assemble(OpCode::OpJumpNotTruthy, vec![10]),
                    // 0004
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    // 0007
                    assembler.assemble(OpCode::OpJump, vec![13]),
                    // 0010
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    // 0013
                    assembler.assemble(OpCode::OpPop, vec![]),
                    // 0014
                    assembler.assemble(OpCode::OpConstant, vec![2]),
                    // 0017
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![
                    Box::new(Integer{value: 10}),
                    Box::new(Integer{value: 20}),
                    Box::new(Integer{value: 3333}),
                ],
            },
        ];

        run_tests(tests)
    }


    #[test]
    fn test_global_let_statements() {
        let assembler = Assembler::new(OpCodes::new());
        let tests = vec![
            TestCase{
                input: "
                let one = 1;
                let two = 2;
                ",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpSetGlobal, vec![0]),
                    assembler.assemble(OpCode::OpConstant, vec![1]),
                    assembler.assemble(OpCode::OpSetGlobal, vec![1]),
                ],
                expected_constants: vec![
                    Box::new(Integer{value: 1}),
                    Box::new(Integer{value: 2}),
                ],
            },
            TestCase{
                input: "
                let one = 1;
                one;
                ",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpSetGlobal, vec![0]),
                    assembler.assemble(OpCode::OpGetGlobal, vec![0]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![
                    Box::new(Integer{value: 1}),
                ],
            },
            TestCase{
                input: "
                let one = 1;
                let two = one;
                two;
                ",
                expected_instructions: vec![
                    assembler.assemble(OpCode::OpConstant, vec![0]),
                    assembler.assemble(OpCode::OpSetGlobal, vec![0]),
                    assembler.assemble(OpCode::OpGetGlobal, vec![0]),
                    assembler.assemble(OpCode::OpSetGlobal, vec![1]),
                    assembler.assemble(OpCode::OpGetGlobal, vec![1]),
                    assembler.assemble(OpCode::OpPop, vec![]),
                ],
                expected_constants: vec![
                    Box::new(Integer{value: 1}),
                ],
            },
        ];

        run_tests(tests)
    }

    fn run_tests(tests: Vec<TestCase>) {
        for t in tests {
            let program = parse(t.input);
            let mut symbol_table = SymbolTable::new();
            let mut constants  = vec![];
            let mut compiler = Compiler::new(&mut symbol_table, &mut constants);
            let error = compiler.compile(program);
            if error.is_err() {
                panic!("Error: {:?}", error.unwrap())
            }
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

        // if actual.len() != expected.len() {
        let disassembler = Disassembler::new(OpCodes::new());
        println!("Expected:\n{}\nActual:\n{}", disassembler.disassemble(expected.clone()), disassembler.disassemble(actual.clone()));
        // }

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
