use std::ops::Deref;

use crate::ast::{Node, IfExpression, BlockStatement, Program};
use crate::object::{Boolean, FALSE, Integer, NULL, Object, ObjectType, TRUE, ReturnValue};

pub struct Evaluator {}

impl Evaluator {
    pub(crate) fn new() -> Self {
        Self {}
    }

    pub fn eval(&self, node: Box<&dyn Node>) -> Option<Box<dyn Object>> {
        node.visit(self)
    }

    pub fn eval_program(&self, program: &Program) -> Option<Box<dyn Object>> {
        let mut object: Option<Box<dyn Object>> = Some(Box::new(NULL));

        for stmt in &program.statements {
            let obj = self.eval(Box::new(stmt.as_node())).unwrap();

            match obj.as_any().downcast_ref::<ReturnValue>() {
                Some(return_value) => {
                    let val = &return_value.value;
                    return Some(val.as_boxed_object())
                },
                None => object = Some(obj)
            }
        }

        object
    }

    pub fn eval_block_statement(&self, block: &BlockStatement) -> Option<Box<dyn Object>> {
        let mut object: Option<Box<dyn Object>> = Some(Box::new(NULL));

        for stmt in &block.statements {
            let evaluated = self.eval(Box::new(stmt.as_node()));

            match evaluated {
                Some(obj) => if obj.get_type() == ObjectType::ReturnValue {
                    return Some(obj.as_boxed_object())
                } else {
                    object = Some(obj.as_boxed_object())
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
            _ => None
        }
    }

    pub fn eval_infix_expression(&self, operator: &str, left: Box<dyn Object>, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
        if left.get_type() == ObjectType::Integer && right.get_type() == ObjectType::Integer {
            return self.eval_integer_infix_expression(operator, left, right);
        }

        match operator {
            "==" => self.native_to_bool(left.eq(right.deref())),
            "!=" => self.native_to_bool(!left.eq(right.deref())),
            _ => None
        }
    }

    pub fn eval_if_expression(&self, expr: &IfExpression) -> Option<Box<dyn Object>> {
        let condition = self.eval(Box::new(expr.condition.as_node())).unwrap();

        if self.is_truthy(condition) {
            self.eval(Box::new(expr.consequence.as_node()))
        } else {
            match &expr.alternative {
                Some(alternative) => self.eval(Box::new(alternative.as_node())),
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
            _ => None
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
            _ => None
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
    use crate::object::{Boolean, Integer, Object, Null};
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

    fn eval(input: &str) -> Box<dyn Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
        let evaluator = Evaluator::new();

        match evaluator.eval(Box::new(program.as_node())) {
            Some(obj) => obj,
            None => panic!("Invalid object")
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