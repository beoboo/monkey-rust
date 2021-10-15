use core::fmt;
use std::any::Any;
use std::fmt::{Display, Formatter};

use crate::evaluator::Evaluator;
use crate::object::{Integer, Object};
use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
    fn as_any(&self) -> &dyn Any;
    fn as_node(&self) -> &dyn Node;
    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>>;
}

pub trait Statement: Node + fmt::Display {}

pub trait Expression: Node + fmt::Display {
    fn value(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        evaluator.eval_statements(&self.statements)
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }

        Ok(())
    }
}

// Statements
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        todo!()
    }
}

impl Statement for LetStatement {}


impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = {};", self.token_literal(), self.name, self.value)
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        todo!()
    }
}

impl Statement for ReturnStatement {}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token_literal(), self.return_value)
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        evaluator.eval(Box::new(self.expression.as_node()))
    }
}

impl Statement for ExpressionStatement {}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        evaluator.eval_statements(&self.statements)
    }
}

impl Statement for BlockStatement {}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }

        Ok(())
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        todo!()
    }
}

impl Expression for Identifier {
    fn value(&self) -> String {
        self.value.clone()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, _evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        Some(Box::new(Integer { value: self.value }))
    }
}

impl Expression for IntegerLiteral {
    fn value(&self) -> String {
        self.value.to_string()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        match evaluator.eval(Box::new(self.right.as_node())) {
            Some(right) => evaluator.eval_prefix_expression(&self.operator, right),
            None => None
        }
    }
}

impl Expression for PrefixExpression {
    fn value(&self) -> String {
        "prefix".to_string()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        let left = match evaluator.eval(Box::new(self.left.as_node())) {
            Some(left) => left,
            None => return None
        };

        let right = match evaluator.eval(Box::new(self.right.as_node())) {
            Some(right) => right,
            None => return None
        };

        evaluator.eval_infix_expression(&self.operator, left, right)
    }
}

impl Expression for InfixExpression {
    fn value(&self) -> String {
        "infix".to_string()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        evaluator.eval_if_expression(self)
    }
}

impl Expression for IfExpression {
    fn value(&self) -> String {
        "if".to_string()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let alt_string = match &self.alternative {
            Some(expr) => format!(" else {}", expr),
            None => "".to_string()
        };

        write!(f, "if {} {}{})", self.condition, self.consequence, alt_string)
    }
}


pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        evaluator.native_to_bool(self.value)
    }
}

impl Expression for BooleanLiteral {
    fn value(&self) -> String {
        self.value.to_string()
    }
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        todo!()
    }
}

impl Expression for FunctionLiteral {
    fn value(&self) -> String {
        "fn".to_string()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut params = vec![];
        for param in &self.parameters {
            params.push(format!("{}", param));
        }
        write!(f, "{}({}){}", self.token_literal(), params.join(", "), self.body)
    }
}

pub struct CallExpression {
    pub token: Token,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> &str {
        return self.token.literal.as_str();
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_node(&self) -> &dyn Node {
        self
    }

    fn visit(&self, evaluator: &Evaluator) -> Option<Box<dyn Object>> {
        todo!()
    }
}

impl Expression for CallExpression {
    fn value(&self) -> String {
        "call".to_string()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut args = vec![];
        for param in &self.arguments {
            args.push(format!("{}", param));
        }
        write!(f, "{}({})", self.function, args.join(", "))
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    use super::*;

    #[test]
    fn string() {
        let program = Program {
            statements: vec![
                Box::new(LetStatement {
                    token: Token::new(TokenType::Let, "let"),
                    name: Identifier {
                        token: Token::new(TokenType::Ident, "myVar"),
                        value: "myVar".to_string(),
                    },
                    value: Box::new(Identifier {
                        token: Token::new(TokenType::Ident, "anotherVar"),
                        value: "anotherVar".to_string(),
                    }),
                })
            ]
        };

        assert_eq!(format!("{}", program), "let myVar = anotherVar;");
    }
}