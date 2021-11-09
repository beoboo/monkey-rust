use num_traits::FromPrimitive;

use crate::compiler::ByteCode;
use crate::object::*;
use crate::op_code::{Instructions, OpCode};
use crate::utils::from_u16;

pub type VMError = String;
pub type VMResult = std::result::Result<(), VMError>;


pub const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;

pub struct VM<'a> {
    constants: Vec<Box<dyn Object>>,
    instructions: Instructions,

    stack: Vec<Box<dyn Object>>,
    globals: &'a mut Vec<Box<dyn Object>>,
    sp: usize,

    true_val: Box<Boolean>,
    false_val: Box<Boolean>,
    null_val: Box<Null>,

    pub last_top: Option<Box<dyn Object>>,
}

impl<'a> VM<'a> {
    pub fn new(bytecode: ByteCode, globals: &'a mut Vec<Box<dyn Object>>) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            globals,
            sp: 0,
            last_top: None,
            true_val: Box::new(Boolean { value: true }),
            false_val: Box::new(Boolean { value: false }),
            null_val: Box::new(Null {}),
        }
    }

    pub fn run(&mut self) -> VMResult {
        let mut ip = 0;
        loop {
            if ip >= self.instructions.len() {
                break;
            }

            let op_code: OpCode = FromPrimitive::from_u8(self.instructions[ip]).unwrap();

            match op_code {
                OpCode::OpConstant => {
                    let index = from_u16(&self.instructions[ip + 1..]);
                    ip += 2;
                    self.push(self.constants[index as usize].clone())?;
                }
                OpCode::OpAdd | OpCode::OpSub | OpCode::OpMul | OpCode::OpDiv => {
                    self.binary_operation(op_code)?;
                }
                OpCode::OpPop => {
                    self.pop();
                }
                OpCode::OpNull => {
                    self.push(self.null_val.clone())?;
                }
                OpCode::OpTrue => {
                    self.push(self.true_val.clone())?;
                }
                OpCode::OpFalse => {
                    self.push(self.false_val.clone())?;
                }
                OpCode::OpEqual | OpCode::OpNotEqual | OpCode::OpGreaterThan => {
                    self.comparison(op_code)?;
                }
                OpCode::OpBang => {
                    self.bang_operation()?;
                }
                OpCode::OpMinus => {
                    self.minus_operation()?;
                }
                OpCode::OpJump => {
                    let pos = from_u16(&self.instructions[ip + 1..]) as usize;
                    ip = pos - 1;
                }
                OpCode::OpJumpNotTruthy => {
                    let pos = from_u16(&self.instructions[ip + 1..]) as usize;
                    ip += 2;

                    let condition = self.pop();
                    if !self.is_truthy(condition) {
                        ip = pos - 1;
                    }
                }
                OpCode::OpSetGlobal => {
                    let index = from_u16(&self.instructions[ip + 1..]) as usize;
                    ip += 2;

                    let global = self.pop();
                    self.globals.insert(index, global);
                }
                OpCode::OpGetGlobal => {
                    let index = from_u16(&self.instructions[ip + 1..]) as usize;
                    ip += 2;

                    let global = match self.globals.get(index) {
                        Some(global) => global.clone(),
                        None=> return Err(VMError::from(format!("No global found in #{} index", index)))
                    };
                    self.push(global)?
                }
                _ => return Err(format!("Unhandled op code: {:?}", op_code))
            }

            ip += 1;
        }

        Ok(())
    }

    fn is_truthy(&self, obj: Box<dyn Object>) -> bool {
        match obj.get_type() {
            ObjectType::Boolean => obj.as_any().downcast_ref::<Boolean>().unwrap().value,
            ObjectType::Null => false,
            _ => true
        }
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
        self.last_top = Some(obj.clone());

        obj
    }

    fn binary_operation(&mut self, op_code: OpCode) -> VMResult {
        let right = self.pop();
        let left = self.pop();

        if left.get_type() == ObjectType::Integer && right.get_type() == ObjectType::Integer {
            let left = left.as_any().downcast_ref::<Integer>().unwrap();
            let right = right.as_any().downcast_ref::<Integer>().unwrap();
            self.binary_integer_operation(op_code, left, right)
        } else {
            Err(format!("unsupported types for binary operation: {:?} {:?} {:?}", left.get_type(), op_code, right.get_type()))
        }
    }

    fn binary_integer_operation(&mut self, op_code: OpCode, left: &Integer, right: &Integer) -> VMResult {
        let result = match op_code {
            OpCode::OpAdd => left.value + right.value,
            OpCode::OpSub => left.value - right.value,
            OpCode::OpMul => left.value * right.value,
            OpCode::OpDiv => left.value / right.value,
            _ => return Err(format!("unknown integer operator: {:?}", op_code))
        };

        self.push(Box::new(Integer { value: result }))
    }

    fn comparison(&mut self, op_code: OpCode) -> VMResult {
        let right = self.pop();
        let left = self.pop();

        if left.get_type() == ObjectType::Integer && right.get_type() == ObjectType::Integer {
            let left = left.as_any().downcast_ref::<Integer>().unwrap();
            let right = right.as_any().downcast_ref::<Integer>().unwrap();
            return self.integer_comparison(op_code, left, right);
        }
        match op_code {
            OpCode::OpEqual => self.push(self.native_bool_to_object(left.eq(&*right))),
            OpCode::OpNotEqual => self.push(self.native_bool_to_object(!left.eq(&*right))),
            _ => Err(format!("unsupported operator: {:?} {:?} {:?}", left.get_type(), op_code, right.get_type())),
        }
    }

    fn integer_comparison(&mut self, op_code: OpCode, left: &Integer, right: &Integer) -> VMResult {
        match op_code {
            OpCode::OpEqual => self.push(self.native_bool_to_object(left.value == right.value)),
            OpCode::OpNotEqual => self.push(self.native_bool_to_object(left.value != right.value)),
            OpCode::OpGreaterThan => self.push(self.native_bool_to_object(left.value > right.value)),
            _ => return Err(format!("unknown integer operator: {:?}", op_code))
        }
    }

    fn bang_operation(&mut self) -> VMResult {
        let operand = self.pop();
        let res = match operand.get_type() {
            ObjectType::Boolean => operand.as_any().downcast_ref::<Boolean>().unwrap().value,
            ObjectType::Null => false,
            _ => true,
        };

        self.push(Box::new(Boolean { value: !res }))
    }

    fn minus_operation(&mut self) -> VMResult {
        let operand = self.pop();
        if operand.get_type() != ObjectType::Integer {
            return Err(format!("unsupported negation for: {:?}", operand.get_type()));
        }

        let integer = operand.as_any().downcast_ref::<Integer>().unwrap();

        self.push(Box::new(Integer { value: -integer.value }))
    }

    fn native_bool_to_object(&self, val: bool) -> Box<dyn Object> {
        if val { self.true_val.clone() } else { self.false_val.clone() }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::compiler::Compiler;
    use crate::disassembler::Disassembler;
    use crate::lexer::Lexer;
    use crate::object::{Boolean, Integer};
    use crate::op_code::OpCodes;
    use crate::parser::Parser;

    use super::*;
    use crate::symbol_table::SymbolTable;

// use crate::code::OpCodes;
    // use crate::disassembler::Disassembler;

    struct TestCase<'a> {
        input: &'a str,
        expected: Box<dyn Object>,
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = build_test_cases(vec![
            ("1", integer(1)),
            ("2", integer(2)),
            ("1 + 2", integer(3)),
            ("1 - 2", integer(-1)),
            ("1 * 2", integer(2)),
            ("4 / 2", integer(2)),
            ("50 / 2 * 2 + 10 - 5", integer(55)),
            ("5 + 5 + 5 + 5 - 10", integer(10)),
            ("2 * 2 * 2 * 2 * 2", integer(32)),
            ("5 * 2 + 10", integer(20)),
            ("5 + 2 * 10", integer(25)),
            ("5 * (2 + 10)", integer(60)),
            ("-5", integer(-5)),
            ("-10", integer(-10)),
            ("-50 + 100 + -50", integer(0)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", integer(50)),
        ]);

        run_tests(tests)
    }

    #[test]
    fn test_boolean_logic() {
        let tests = build_test_cases(vec![
            ("true", boolean(true)),
            ("false", boolean(false)),
            ("1 < 2", boolean(true)),
            ("1 > 2", boolean(false)),
            ("1 < 1", boolean(false)),
            ("1 > 1", boolean(false)),
            ("1 == 1", boolean(true)),
            ("1 != 1", boolean(false)),
            ("1 == 2", boolean(false)),
            ("1 != 2", boolean(true)),
            ("true == true", boolean(true)),
            ("false == false", boolean(true)),
            ("true == false", boolean(false)),
            ("true != false", boolean(true)),
            ("false != true", boolean(true)),
            ("(1 < 2) == true", boolean(true)),
            ("(1 < 2) == false", boolean(false)),
            ("(1 > 2) == true", boolean(false)),
            ("(1 > 2) == false", boolean(true)),
            ("!true", boolean(false)),
            ("!false", boolean(true)),
            ("!5", boolean(false)),
            ("!!true", boolean(true)),
            ("!!false", boolean(false)),
            ("!!5", boolean(true)),
            ("!(if (false) { 10 })", boolean(true)),
        ]);

        run_tests(tests)
    }

    #[test]
    fn test_conditionals() {
        let tests = build_test_cases(vec![
            ("if (true) { 10 }", integer(10)),
            ("if (true) { 10 } else { 20 }", integer(10)),
            ("if (false) { 10 } else { 20 }", integer(20)),
            ("if (1) { 10 }", integer(10)),
            ("if (1 < 2) { 10 }", integer(10)),
            ("if (1 < 2) { 10 } else { 20 }", integer(10)),
            ("if (1 > 2) { 10 } else { 20 }", integer(20)),
            ("if (1 > 2) { 10 }", Box::new(Null {})),
            ("if (false) { 10 }", Box::new(Null {})),
            ("if (if (false) { 10 }) { 10 } else { 20 }", integer(20)),
        ]);

        run_tests(tests)
    }


    #[test]
    fn test_let_statements() {
        let tests = build_test_cases(vec![
            ("let one = 1; one", integer(1)),
            ("let one = 1; let two = 2; one + two", integer(3)),
            ("let one = 1; let two = one + one; one + two", integer(3)),
        ]);

        run_tests(tests)
    }

    fn build_test_cases(tests: Vec<(&str, Box<dyn Object>)>) -> Vec<TestCase> {
        tests.iter().map(|(input, expected)| TestCase { input: *input, expected: expected.clone_box() })
            .collect::<Vec<_>>()
    }

    fn integer(value: i64) -> Box<dyn Object> {
        Box::new(Integer { value })
    }

    fn boolean(value: bool) -> Box<dyn Object> {
        Box::new(Boolean { value })
    }


    fn run_tests(tests: Vec<TestCase>) {
        for t in tests {
            let program = parse(t.input);
            let mut symbol_table = SymbolTable::new();
            let mut constants  = vec![];
            let mut compiler = Compiler::new(&mut symbol_table, &mut constants);
            let error = compiler.compile(program);
            assert!(error.is_ok());

            let bytecode = compiler.bytecode();
            let disassembler = Disassembler::new(OpCodes::new());
            println!("{}", disassembler.disassemble(bytecode.instructions.clone()));

            // for (i, c) in bytecode.constants.iter().enumerate() {
            //     println!("#{}: {}", i, c.inspect());
            // }
            let mut globals = vec![];
            let mut vm = VM::new(bytecode, &mut globals);
            let res = vm.run();
            if res.is_err() {
                println!("{:?}", res);
            }
            assert!(res.is_ok());

            let top = vm.last_top.unwrap();

            assert_eq!(top.inspect(), t.expected.inspect());
        }
    }

    fn parse(input: &str) -> Box<dyn Node> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap_or_else(|| panic!("Invalid program"))
    }
}