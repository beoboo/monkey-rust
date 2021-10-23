use std::collections::HashMap;
use std::ops::Deref;

use crate::ast::*;
use crate::environment::Environment;
use crate::object::*;
use crate::builtins::*;

pub struct Evaluator {
    builtins: HashMap<String, Builtin>,
}


impl Evaluator {
    pub(crate) fn new() -> Self {
        let builtins: HashMap<String, Builtin> = vec![
            ("len".to_string(), Builtin { function: len_builtin }),
            ("puts".to_string(), Builtin { function: puts_builtin }),
            ("first".to_string(), Builtin { function: first_builtin }),
            ("last".to_string(), Builtin { function: last_builtin }),
            ("rest".to_string(), Builtin { function: rest_builtin }),
            ("push".to_string(), Builtin { function: push_builtin }),
        ].into_iter()
            .collect();

        Self {
            builtins
        }
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
                    return Some(val.clone());
                }
                ObjectType::Error => {
                    return Some(obj);
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
                    ObjectType::ReturnValue | ObjectType::Error => return Some(obj.clone()),
                    _ => object = Some(obj.clone())
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
            return Some(Box::new(Error::new(format!("type mismatch: {:?} {} {:?}", left.get_type(), operator, right.get_type()))));
        }
        if left.get_type() == ObjectType::Integer && right.get_type() == ObjectType::Integer {
            return self.eval_integer_infix_expression(operator, left, right);
        }
        if left.get_type() == ObjectType::String && right.get_type() == ObjectType::String {
            return self.eval_string_infix_expression(operator, left, right);
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
            return condition;
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

    fn eval_string_infix_expression(&self, operator: &str, left: Box<dyn Object>, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
        if operator != "+" {
            return Some(Box::new(Error::new(format!("unknown operator: {:?} {} {:?}", left.get_type(), operator, right.get_type()))));
        }

        let left = left.as_any().downcast_ref::<StringE>().unwrap();
        let right = right.as_any().downcast_ref::<StringE>().unwrap();

        Some(Box::new(StringE { value: left.value.to_owned() + right.value.as_str() }))
    }

    pub fn eval_expressions(&self, exprs: Vec<Box<dyn Expression>>, env: &mut Environment) -> Vec<Box<dyn Object>> {
        let mut result: Vec<Box<dyn Object>> = vec![];
        for e in exprs {
            let evaluated = self.eval(Box::new(e.as_node()), env);
            if is_error(&evaluated) {
                return vec![evaluated.unwrap()];
            }

            match evaluated {
                Some(expr) => result.push(expr),
                None => {}
            }
        }

        result
    }

    pub fn eval_function_call(&self, function: Box<dyn Object>, args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
        match function.get_type() {
            ObjectType::Function => {
                let function = function.as_any().downcast_ref::<Function>().unwrap();
                let mut extended_env = self.extend_function_env(function, args);
                let evaluated = self.eval(Box::new(function.body.as_node()), &mut extended_env)?;

                self.unwrap_return_value(evaluated)
            }
            ObjectType::Builtin => {
                let builtin = function.as_any().downcast_ref::<Builtin>().unwrap();
                (builtin.function)(args)
            }
            _ => Some(Box::new(Error::new(format!("not a function: {:?}", function.get_type()))))
        }
    }

    fn extend_function_env(&self, function: &Function, args: Vec<Box<dyn Object>>) -> Environment {
        let mut env = Environment::enclose(function.env.clone());

        for (idx, param) in function.parameters.iter().enumerate() {
            env.set(param.value.clone(), args[idx].clone());
        }

        env
    }

    fn unwrap_return_value(&self, object: Box<dyn Object>) -> Option<Box<dyn Object>> {
        match object.as_any().downcast_ref::<ReturnValue>() {
            Some(return_value) => Some(return_value.value.clone()),
            None => Some(object)
        }
    }

    pub fn eval_identifier(&self, identifier: &Identifier, env: &mut Environment) -> Option<Box<dyn Object>> {
        match env.get(&identifier.value) {
            Some(identifier) => Some(identifier.clone()),
            None => {
                match self.builtins.get(&identifier.value) {
                    Some(builtin) => Some(Box::new(builtin.clone())),
                    None => Some(Box::new(Error::new(format!("identifier not found: {}", &identifier.value))))
                }
            }
        }
    }

    pub fn eval_index_expression(&self, left: Box<dyn Object>, index: Box<dyn Object>) -> Option<Box<dyn Object>> {
        if left.get_type() == ObjectType::Array && index.get_type() == ObjectType::Integer {
            let array = left.as_any().downcast_ref::<Array>()?;
            let index = index.as_any().downcast_ref::<Integer>()?.value;

            let max = (array.elements.len() - 1) as i64;

            if index < 0 || index > max {
                return Some(Box::new(NULL));
            }

            Some(array.elements[index as usize].clone())
        } else if left.get_type() == ObjectType::Map {
            let map = left.as_any().downcast_ref::<Map>()?;
            let key = match index.map_key() {
                Some(key) => key,
                None => return Some(Box::new(Error::new(format!("{:?} not usable as map key", index.get_type()))))
            };

            let pair = map.pairs.get(&key)?;

            Some(pair.value.clone())
        } else {
            Some(Box::new(Error::new(format!("{:?} index operator not supported for {:?}", index.get_type(), left.get_type()))))
        }
    }

    pub fn eval_map_literal(&self, map: &MapLiteral, env: &mut Environment) -> Option<Box<dyn Object>> {
        let mut pairs: HashMap<MapKey, Pair> = HashMap::new();

        for (key, value) in &map.pairs {
            let key = self.eval(Box::new(key.as_node()), env);
            if is_error(&key) {
                return key;
            }

            let key = key.unwrap();
            let value = self.eval(Box::new(value.as_node()), env);
            if is_error(&value) {
                return value;
            }

            let map_key = match key.map_key() {
                Some(key) => key,
                None => return None
            };

            pairs.insert(map_key, Pair { key: key, value: value.unwrap() });
        }

        Some(Box::new(Map { pairs }))
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
            Test {
                input: "
if (10 > 1) {
 if (10 > 1) {
   return 10;
 }
}
return 1;
            ",
                expected: 10,
            },
            Test {
                input: "
let f = fn(x) {
  return x;
  x + 10;
};
f(10);",
                expected: 10,
            },
            Test {
                input: "
let f = fn(x) {
   let result = x + 10;
   return result;
   return 10;
};
f(10);",
                expected: 20,
            },
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
            Test { input: "5 + true;", expected: "type mismatch: Integer + Boolean" },
            Test { input: "5 + true; 5;", expected: "type mismatch: Integer + Boolean" },
            Test { input: "-true", expected: "unknown operator: -Boolean" },
            Test { input: "true + true;", expected: "unknown operator: Boolean + Boolean" },
            Test { input: "5; true + false; 5;", expected: "unknown operator: Boolean + Boolean" },
            Test { input: "if (10 > 1) { true + false; }", expected: "unknown operator: Boolean + Boolean" },
            Test {
                input: "
if (10 > 1) {
    if (10 > 1) {
        true + false;
    }
}
",
                expected: "unknown operator: Boolean + Boolean",
            },
            Test { input: "foobar", expected: "identifier not found: foobar" },
            Test { input: "\"hello\" - \"world\"", expected: "unknown operator: String - String" },
            Test { input: "999[1]", expected: "Integer index operator not supported for Integer" },
            Test { input: "{\"name\": \"Monkey\"}[fn(x) { x }];", expected: "Function not usable as map key" },
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
            Test { input: "let a = 5; a;", expected: 5 },
            Test { input: "let a = 5 * 5; a;", expected: 25 },
            Test { input: "let a = 5; let b = a; b;", expected: 5 },
            Test { input: "let a = 5; let b = a; let c = a + b + 5; c;", expected: 15 },
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_integer_object(evaluated, test.expected);
        }
    }

    #[test]
    fn eval_function() {
        let input = "fn(x) { x + 2; }";
        let evaluated = eval(input);
        let function = evaluated.as_any().downcast_ref::<Function>().unwrap_or_else(|| panic!("Invalid function"));

        assert_eq!(function.parameters.len(), 1);
        assert_eq!(function.parameters[0].to_string(), "x");
        assert_eq!(function.body.to_string(), "(x + 2)")
    }

    #[test]
    fn eval_function_call() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test { input: "let identity = fn(x) { x; }; identity(5);", expected: 5 },
            Test { input: "let identity = fn(x) { return x; }; identity(5);", expected: 5 },
            Test { input: "let double = fn(x) { x * 2; }; double(5);", expected: 10 },
            Test { input: "let add = fn(x, y) { x + y; }; add(5, 5);", expected: 10 },
            Test { input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", expected: 20 },
            Test { input: "fn(x) { x; }(5)", expected: 5 },
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_integer_object(evaluated, test.expected);
        }
    }

    #[test]
    fn eval_enclosed_environment() {
        let input = "\
let first = 10;
let second = 10;
let third = 10;

let ourFunction = fn(first) {
  let second = 20;

  first + second + third;
};

ourFunction(20) + first + second;";

        assert_integer_object(eval(input), 70);
    }

    #[test]
    fn eval_closure() {
        let input = "
let newAdder = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
";

        assert_integer_object(eval(input), 4);
    }

    #[test]
    fn eval_string_literal() {
        let input = "\"Hello World!\"";

        let evaluated = eval(input);
        let string = evaluated.as_any().downcast_ref::<StringE>()
            .unwrap_or_else(|| panic!("Not a string"));

        assert_eq!(string.value, "Hello World!");
    }

    #[test]
    fn eval_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";

        let evaluated = eval(input);
        let string = evaluated.as_any().downcast_ref::<StringE>()
            .unwrap_or_else(|| panic!("Not a string"));

        assert_eq!(string.value, "Hello World!");
    }

    #[test]
    fn eval_builtin_functions() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let tests = vec![
            Test { input: "len(\"\")", expected: "0" },
            Test { input: "len(\"four\")", expected: "4" },
            Test { input: "len(\"hello world\")", expected: "11" },
            Test { input: "len(1)", expected: "Error: argument to \"len\" not supported, got Integer" },
            Test { input: "len(\"one\", \"two\")", expected: "Error: wrong number of arguments. got=2, want=1" },
            Test { input: "len([1, 2, 3])", expected: "3" },
            Test { input: "len([])", expected: "0" },
            Test { input: "puts(\"hello\", \"world!\")", expected: "null" },
            Test { input: "first([1, 2, 3])", expected: "1" },
            Test { input: "first([])", expected: "null" },
            Test { input: "first(1)", expected: "Error: argument to \"first\" must be Array, got Integer" },
            Test { input: "last([1, 2, 3])", expected: "3" },
            Test { input: "last([])", expected: "null" },
            Test { input: "last(1)", expected: "Error: argument to \"last\" must be Array, got Integer" },
            Test { input: "rest([1, 2, 3])", expected: "[2, 3]" },
            Test { input: "rest([])", expected: "null" },
            Test { input: "push([], 1)", expected: "[1]" },
            Test { input: "push(1, 1)", expected: "Error: argument to \"push\" must be Array, got Integer" },
        ];

        for test in tests {
            let evaluated = eval(test.input);

            assert_eq!(evaluated.inspect().as_str(), test.expected);
        }
    }

    #[test]
    fn eval_array() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = eval(input);
        let array = evaluated.as_any().downcast_ref::<Array>().unwrap_or_else(|| panic!("Invalid array"));

        assert_eq!(array.elements.len(), 3);
        assert_integer_object(array.elements[0].clone(), 1);
        assert_integer_object(array.elements[1].clone(), 4);
        assert_integer_object(array.elements[2].clone(), 6);
    }

    #[test]
    fn eval_index_expressions() {
        struct Test<'a> {
            input: &'a str,
            expected: Option<i64>,
        }

        let tests = vec![
            Test { input: "[1, 2, 3][0]", expected: Some(1) },
            Test { input: "[1, 2, 3][1]", expected: Some(2) },
            Test { input: "[1, 2, 3][2]", expected: Some(3) },
            Test { input: "let i = 0; [1][i];", expected: Some(1) },
            Test { input: "[1, 2, 3][1 + 1]", expected: Some(3) },
            Test { input: "let myArray = [1, 2, 3]; myArray[2];", expected: Some(3) },
            Test { input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", expected: Some(6) },
            Test { input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", expected: Some(2) },
            Test { input: "[1, 2, 3][3]", expected: None },
            Test { input: "[1, 2, 3][-1]", expected: None },
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
    fn eval_map() {
        let input = "let two = \"two\"; \
{
    \"one\": 10 - 9, \
    two: 1 + 1, \
    \"thr\" + \"ee\": 6 / 2,
    4: 4,
    true: 5,
    false: 6
}";
        let evaluated = eval(input);
        let map = evaluated.as_any().downcast_ref::<Map>().unwrap_or_else(|| panic!("Invalid map"));

        let expected: HashMap<MapKey, i64> = vec![
            (StringE { value: "one".to_string() }.map_key().unwrap(), 1),
            (StringE { value: "two".to_string() }.map_key().unwrap(), 2),
            (StringE { value: "three".to_string() }.map_key().unwrap(), 3),
            (Integer { value: 4 }.map_key().unwrap(), 4),
            (Boolean { value: true }.map_key().unwrap(), 5),
            (Boolean { value: false }.map_key().unwrap(), 6),
        ]
            .into_iter()
            .collect();

        assert_eq!(map.pairs.len(), expected.len());

        for (key, value) in expected {
            let pair = map.pairs.get(&key).unwrap_or_else(|| panic!("No pair for the given key"));

            assert_integer_object(pair.value.clone(), value);
        }
    }

    #[test]
    fn eval_map_index_expressions() {
        struct Test<'a> {
            input: &'a str,
            expected: Option<i64>,
        }

        let tests = vec![
            Test { input: "{\"foo\": 5}[\"foo\"]", expected: Some(5) },
            Test { input: "{\"foo\": 5}[\"bar\"]", expected: None },
            Test { input: "let key = \"foo\"; {\"foo\": 5}[key]", expected: Some(5) },
            Test { input: "{}[\"foo\"]", expected: None },
            Test { input: "{5: 5}[5]", expected: Some(5) },
            Test { input: "{true: 5}[true]", expected: Some(5) },
            Test { input: "{false: 5}[false]", expected: Some(5) },
        ];

        for test in tests {
            let evaluated = eval(test.input);

            match test.expected {
                Some(n) => assert_integer_object(evaluated, n),
                None => assert_null_object(evaluated)
            }
        }
    }

    fn eval(input: &str) -> Box<dyn Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid program"));
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