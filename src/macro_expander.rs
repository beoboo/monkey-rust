use crate::ast::*;
use crate::environment::Environment;
use crate::object::*;
use crate::utils::is_macro_definition;
use crate::evaluator::Evaluator;
use crate::modifier::{ModifierImpl, Modifier};

pub struct MacroExpander {}


impl MacroExpander {
    pub fn new() -> Self {
        Self {}
    }

    pub fn define_macros(&self, program: &mut Box<Program>, env: &mut Environment) {
        let mut definitions = vec![];

        for (i, statement) in program.statements.iter().enumerate() {
            if is_macro_definition(statement) {
                self.add_macro(statement, env);
                definitions.push(i);
            }
        }

        for i in definitions.iter().rev() {
            program.statements.remove(*i);
        }
    }

    pub fn expand_macros(&self, program: &Box<Program>, env: &mut Environment) -> Box<dyn Node> {
        let mut unquote_cls = Box::new(|n: Box<dyn Node>| -> Box<dyn Node> {
            let call = match n.as_any().downcast_ref::<CallExpression>() {
                Some(expression) => expression,
                None => return n.clone()
            };

            let macro_ = match self.get_macro(call, env) {
                Some(macro_) => macro_,
                None => return n.clone()
            };

            let args = self.quote_args(call);
            let mut env = self.extend_env(macro_, args);
            let evaluator = Evaluator::new();
            let evaluated = evaluator.eval(Box::new(macro_.body.as_node()), &mut env).unwrap_or_else(|| panic!("Empty macro body"));
            match evaluated.as_any().downcast_ref::<Quote>() {
                Some(quote) => quote.to_node(),
                None => panic!("We only support returning AST nodes from macros")
            }
        });

        let mut modifier = ModifierImpl { modifier_fn: &mut unquote_cls };

        modifier.modify(program.clone())
    }

    fn add_macro(&self, stmt: &Box<dyn Statement>, env: &mut Environment) {
        let let_statement = stmt.as_any().downcast_ref::<LetStatement>().unwrap();
        let macro_literal = let_statement.value.as_any().downcast_ref::<MacroLiteral>().unwrap();

        let macro_obj = Macro {
            parameters: macro_literal.parameters.clone(),
            env: env.clone(),
            body: macro_literal.body.clone(),
        };

        env.set(let_statement.name.value.to_string(), Box::new(macro_obj));
    }

    fn get_macro<'a>(&self, expr: &CallExpression, env: &'a Environment) -> Option<&'a Macro> {
        let identifier = match expr.function.as_any().downcast_ref::<Identifier>() {
            Some(identifier) => identifier,
            None => return None
        };

        let obj = match env.get(&identifier.value) {
            Some(obj) => obj,
            None => return None,
        };

        obj.as_any().downcast_ref::<Macro>().clone()
    }

    fn quote_args(&self, expr: &CallExpression) -> Vec<Quote> {
        let mut args = vec![];

        for arg in &expr.arguments {
            args.push(Quote{node: Some(arg.clone_node())})
        }

        args
    }

    fn extend_env(&self, macro_: &Macro, args: Vec<Quote>) -> Environment {
        let mut extended = Environment::enclose(macro_.env.clone());

        for (i, param) in macro_.parameters.iter().enumerate() {
            extended.set(param.value.to_string(), Box::new(args[i].clone()));
        }

        extended
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn define_macro() {
        let input = "
        let number = 1;
        let function = fn(x, y) { x + y; };
        let mymacro = macro(x, y) { x + y; }
        ";

        let mut env = Environment::new();
        let mut program = parse_program(input);
        let expander = MacroExpander::new();
        expander.define_macros(&mut program, &mut env);

        assert_eq!(program.statements.len(), 2);
        assert!(env.get(&"number".to_string()).is_none());
        assert!(env.get(&"function".to_string()).is_none());

        let my_macro = env.get(&"mymacro".to_string()).unwrap().as_any().downcast_ref::<Macro>().unwrap_or_else(|| panic!("Invalid macro"));

        assert_eq!(my_macro.parameters.len(), 2);
        assert_eq!(my_macro.parameters[0].to_string(), "x");
        assert_eq!(my_macro.parameters[1].to_string(), "y");
        assert_eq!(my_macro.body.to_string(), "(x + y)");
    }

    #[test]
    fn expand_macro() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let tests = vec![
            Test {
                input: "
                    let infixExpression = macro() { quote(1 + 2); };

                    infixExpression();
                    ",
                expected: "(1 + 2)",
            },
            Test {
                input: "
                    let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };

                    reverse(2 + 2, 10 - 5);
                    ",
                expected: "(10 - 5) - (2 + 2)",
            },
            Test {
                input: "
                    let unless = macro(condition, consequence, alternative) {
                        quote(if (!(unquote(condition))) {
                            unquote(consequence);
                        } else {
                            unquote(alternative);
                        });
                    };

                    unless(10 > 5, puts(\"not greater\"), puts(\"greater\"));            \
                    ",
                expected: "if (!(10 > 5)) { puts(\"not greater\") } else { puts(\"greater\") }",
            },
        ];

        for test in tests {
            let expected = parse_program(test.expected);
            let mut program = parse_program(test.input);

            let mut env = Environment::new();
            let expander = MacroExpander::new();
            expander.define_macros(&mut program, &mut env);
            let expanded = expander.expand_macros(&program, &mut env);

            assert_eq!(expanded.to_string(), expected.to_string());
        }
    }

    fn parse_program(input: &str) -> Box<Program> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap_or_else(|| panic!("Invalid program"))
    }
}