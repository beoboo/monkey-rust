use std::ops::Deref;

use crate::ast::{Node, IfExpression, BlockStatement, Program};
use crate::object::{Boolean, FALSE, Integer, NULL, Object, ObjectType, TRUE, ReturnValue, Error, is_error};
use crate::environment::Environment;

pub struct Evaluator {}

impl Evaluator {
    pub(crate) fn new() -> Self {
        Self {}
    }

    pub fn eval(&self, node: Box<&dyn Node>, env: &mut Environment) -> Option<Box<dyn Object>> {
        node.visit(self, env)
    }

    pub fn eval_program(&self, program: &Program, env: &mut Environment) -> Option<Box<dyn Object>> {
        let mut object: Option<Box<dyn Object>> = Some(Box::new(NULL));

        for stmt in &program.statements {
            let obj = match self.eval(Box::new(stmt.as_node()), env) {
                Some(obj) => obj,
                None => continue
            };

            match obj.get_type() {
                ObjectType::ReturnValue => {
                    let return_value = obj.as_any().downcast_ref::<ReturnValue>().unwrap();
                    let val = &return_value.value;
                    return Some(val.as_boxed_object())
                }
                ObjectType::Error => {
                    return Some(obj)
                }
                _ => object = Some(obj)
            }
        }

        object
    }

    pub fn eval_block_statement(&self, block: &BlockStatement, env: &mut Environment) -> Option<Box<dyn Object>> {
        let mut object: Option<Box<dyn Object>> = Some(Box::new(NULL));

        for stmt in &block.statements {
            let evaluated = self.eval(Box::new(stmt.as_node()), env);

            match evaluated {
                Some(obj) => match obj.get_type() {
                    ObjectType::ReturnValue | ObjectType::Error => return Some(obj.as_boxed_object()),
                    _ => object = Some(obj.as_boxed_object())
                }
                None => {}
            }
        }

        object
    }

    pub fn eval_prefix_expression(&self, operator: &str, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prefix_operator_expression(right),
            _ => Some(Box::new(Error::new(format!("unknown operator: {}{:?}", operator, right.get_type()))))
        }
    }

    pub fn eval_infix_expression(&self, operator: &str, left: Box<dyn Object>, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
        if left.get_type() != right.get_type() {
            return Some(Box::new(Error::new(format!("type mismatch: {:?} {} {:?}", left.get_type(), operator, right.get_type()))))
        }
        if left.get_type() == ObjectType::Integer && right.get_type() == ObjectType::Integer {
            return self.eval_integer_infix_expression(operator, left, right);
        }

        match operator {
            "==" => self.native_to_bool(left.eq(right.deref())),
            "!=" => self.native_to_bool(!left.eq(right.deref())),
            _ => Some(Box::new(Error::new(format!("unknown operator: {:?} {} {:?}", left.get_type(), operator, right.get_type()))))
        }
    }

    pub fn eval_if_expression(&self, expr: &IfExpression, env: &mut Environment) -> Option<Box<dyn Object>> {
        let condition = self.eval(Box::new(expr.condition.as_node()), env);
        if is_error(&condition) {
            return condition
        }

        if self.is_truthy(condition.unwrap()) {
            self.eval(Box::new(expr.consequence.as_node()), env)
        } else {
            match &expr.alternative {
                Some(alternative) => self.eval(Box::new(alternative.as_node()), env),
                None => Some(Box::new(NULL))
            }
        }
    }

    fn eval_bang_operator_expression(&self, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
        let res = match right.get_type() {
            ObjectType::Boolean => match right.as_any().downcast_ref::<Boolean>().unwrap().value {
                true => FALSE,
                false => TRUE
            },
            ObjectType::Null => TRUE,
            _ => FALSE
        };

        Some(Box::new(res))
    }

    fn eval_minus_prefix_operator_expression(&self, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
        match right.get_type() {
            ObjectType::Integer => {
                let value = right.as_any().downcast_ref::<Integer>().unwrap().value;
                Some(Box::new(Integer { value: -value }))
            }
            _ => Some(Box::new(Error::new(format!("unknown operator: -{:?}", right.get_type()))))
        }
    }

    fn eval_integer_infix_expression(&self, operator: &str, left: Box<dyn Object>, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
        let left = left.as_any().downcast_ref::<Integer>().unwrap();
        let right = right.as_any().downcast_ref::<Integer>().unwrap();

        match operator {
            "+" => Some(Box::new(Integer { value: left.value + right.value })),
            "-" => Some(Box::new(Integer { value: left.value - right.value })),
            "*" => Some(Box::new(Integer { value: left.value * right.value })),
            "/" => Some(Box::new(Integer { value: left.value / right.value })),
            "<" => self.native_to_bool(left.value < right.value),
            ">" => self.native_to_bool(left.value > right.value),
            "==" => self.native_to_bool(left.value == right.value),
            "!=" => self.native_to_bool(left.value != right.value),
            _ => Some(Box::new(Error::new(format!("unknown operator: {:?} {} {:?}", left.get_type(), operator, right.get_type()))))
        }
    }

    pub fn native_to_bool(&self, input: bool) -> Option<Box<dyn Object>> {
        match input {
            true => Some(Box::new(TRUE)),
            false => Some(Box::new(FALSE))
        }
    }

    fn is_truthy(&self, obj: Box<dyn Object>) -> bool {
        match obj.get_type() {
            ObjectType::Boolean => obj.as_any().downcast_ref::<Boolean>().unwrap().value,
            ObjectType::Null => false,
            _ => true
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::object::{Boolean, Integer, Object, Null, Error};
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn eval_integer_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test { input: "5", expected: 5 },
            Test { input: "10", expected: 10 },
            Test { input: "-5", expected: -5 },
            Test { input: "-10", expected: -10 },
            Test { input: "5 + 5 + 5 + 5 - 10", expected: 10 },
            Test { input: "2 * 2 * 2 * 2 * 2", expected: 32 },
            Test { input: "-50 + 100 + -50", expected: 0 },
            Test { input: "5 * 2 + 10", expected: 20 },
            Test { input: "5 + 2 * 10", expected: 25 },
            Test { input: "20 + 2 * -10", expected: 0 },
            Test { input: "50 / 2 * 2 + 10", expected: 60 },
            Test { input: "2 * (5 + 10)", expected: 30 },
            Test { input: "3 * 3 * 3 + 10", expected: 37 },
            Test { input: "3 * (3 * 3) + 10", expected: 37 },
            Test { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: 50 },
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_integer_object(evaluated, test.expected);
        }
    }

    #[test]
    fn eval_boolean_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }

        let tests = vec![
            Test { input: "true", expected: true },
            Test { input: "false", expected: false },
            Test { input: "1 < 2", expected: true },
            Test { input: "1 > 2", expected: false },
            Test { input: "1 < 1", expected: false },
            Test { input: "1 > 1", expected: false },
            Test { input: "1 == 1", expected: true },
            Test { input: "1 != 1", expected: false },
            Test { input: "1 == 2", expected: false },
            Test { input: "1 != 2", expected: true },
            Test { input: "true == true", expected: true },
            Test { input: "false == false", expected: true },
            Test { input: "true == false", expected: false },
            Test { input: "true != false", expected: true },
            Test { input: "false != true", expected: true },
            Test { input: "(1 < 2) == true", expected: true },
            Test { input: "(1 < 2) == false", expected: false },
            Test { input: "(1 > 2) == true", expected: false },
            Test { input: "(1 > 2) == false", expected: true },
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_boolean_object(evaluated, test.expected);
        }
    }

    #[test]
    fn eval_bang_operator() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }

        let tests = vec![
            Test { input: "!true", expected: false },
            Test { input: "!false", expected: true },
            Test { input: "!5", expected: false },
            Test { input: "!!true", expected: true },
            Test { input: "!!false", expected: false },
            Test { input: "!!5", expected: true },
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_boolean_object(evaluated, test.expected);
        }
    }

    #[test]
    fn eval_if_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: Option<i64>,
        }

        let tests = vec![
            Test { input: "if (true) { 10 }", expected: Some(10) },
            Test { input: "if (false) { 10 }", expected: None },
            Test { input: "if (1) { 10 }", expected: Some(10) },
            Test { input: "if (1 < 2) { 10 }", expected: Some(10) },
            Test { input: "if (1 > 2) { 10 }", expected: None },
            Test { input: "if (1 > 2) { 10 } else { 20 }", expected: Some(20) },
            Test { input: "if (1 < 2) { 10 } else { 20 }", expected: Some(10) },
        ];

        for test in tests {
            let evaluated = eval(test.input);
            match test.expected {
                Some(n) => assert_integer_object(evaluated, n),
                None => assert_null_object(evaluated)
            }
        }
    }

    #[test]
    fn eval_return_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test { input: "return 10;", expected: 10 },
            Test { input: "return 10; 9;", expected: 10 },
            Test { input: "return 2 * 5; 9;", expected: 10 },
            Test { input: "9; return 2 * 5; 9;", expected: 10 },
            Test { input: "
if (10 > 1) {
 if (10 > 1) {
   return 10;
 }
}
return 1;
            ", expected: 10}
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_integer_object(evaluated, test.expected);
        }
    }

    #[test]
    fn eval_error_handling() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let tests = vec![
            Test{ input: "5 + true;", expected: "type mismatch: Integer + Boolean"},
            Test{ input: "5 + true; 5;", expected: "type mismatch: Integer + Boolean"},
            Test{ input: "-true", expected: "unknown operator: -Boolean"},
            Test{ input: "true + true;", expected: "unknown operator: Boolean + Boolean"},
            Test{ input: "5; true + false; 5;", expected: "unknown operator: Boolean + Boolean"},
            Test{ input: "if (10 > 1) { true + false; }", expected: "unknown operator: Boolean + Boolean"},
            Test{ input: "
if (10 > 1) {
    if (10 > 1) {
        true + false;
    }
}
", expected: "unknown operator: Boolean + Boolean"},
            Test{ input: "foobar", expected: "identifier not found: foobar"},
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_eq!(evaluated.get_type(), ObjectType::Error);

            let error_obj = evaluated.as_any().downcast_ref::<Error>().unwrap();
            assert_eq!(error_obj.message, test.expected);
        }
    }

    #[test]
    fn eval_let_statement() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test{ input:"let a = 5; a;", expected: 5},
            // Test{ input:"let a = 5 * 5; a;", expected: 25},
            // Test{ input:"let a = 5; let b = a; b;", expected: 5},
            // Test{ input:"let a = 5; let b = a; let c = a + b + 5; c;", expected: 15},
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_integer_object(evaluated, test.expected);
        }
    }

    fn eval(input: &str) -> Box<dyn Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
        let evaluator = Evaluator::new();
        let mut environment = Environment::new();

        match evaluator.eval(Box::new(program.as_node()), &mut environment) {
            Some(obj) => obj,
            None => panic!("Invalid program: {}", program)
        }
    }

    fn assert_integer_object(evaluated: Box<dyn Object>, expected: i64) {
        let result = evaluated.as_any().downcast_ref::<Integer>()
            .unwrap_or_else(|| { panic!("Not an integer") });

        assert_eq!(result.value, expected);
    }

    fn assert_boolean_object(evaluated: Box<dyn Object>, expected: bool) {
        let result = evaluated.as_any().downcast_ref::<Boolean>()
            .unwrap_or_else(|| { panic!("Not a boolean") });

        assert_eq!(result.value, expected);
    }

    fn assert_null_object(evaluated: Box<dyn Object>) {
        evaluated.as_any().downcast_ref::<Null>()
            .unwrap_or_else(|| { panic!("Not a null") });
    }
}