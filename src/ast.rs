use crate::token::Token;
use std::any::Any;

pub trait Node {
    fn token_literal(&self) -> &str;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
}

pub trait Expression: Node {
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
}

impl Expression for Identifier {
}

pub struct DummyExpression {

}

impl Node for DummyExpression {
    fn token_literal(&self) -> &str {
        "dummy"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for DummyExpression {

}

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
}

impl Statement for LetStatement {
}
