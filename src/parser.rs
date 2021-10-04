use crate::ast::{DummyExpression, Identifier, LetStatement, Program, Statement, ReturnStatement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    next_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let next_token = lexer.next_token();
        Self { lexer: lexer, cur_token, next_token, errors: vec![] }
    }

    fn next_token(&mut self) {
        let next_token = self.next_token.clone();
        self.cur_token = next_token;
        self.next_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut statements = vec![];

        while !self.cur_token_is(TokenType::EOF) {
            let statement = self.parse_statement();

            match statement {
                Some(stmt) => statements.push(stmt),
                _ => {}
            }

            self.next_token();
        }

        Some(Program {
            statements
        })
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.token_type {
            TokenType::Let => {
                match self.parse_let_statement() {
                    Some(stmt) => Some(Box::new(stmt)),
                    _ => None,
                }
            }
            TokenType::Return => {
                match self.parse_return_statement() {
                    Some(stmt) => Some(Box::new(stmt)),
                    _ => None,
                }
            }
            _ => None
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let let_token = self.cur_token.clone();
        if !self.expect_next(TokenType::Ident) {
            return None;
        }

        let id_token = self.cur_token.clone();
        let identifier = Identifier { token: id_token.clone(), value: id_token.literal.clone() };

        if !self.expect_next(TokenType::Assign) {
            return None;
        }

        while !self.cur_token_is(TokenType::Semicolon) {
            // println!("Found semicolon");
            self.next_token();
        }

        Some(LetStatement {
            token: let_token,
            name: identifier,
            value: Box::new(DummyExpression {}),
        })
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let token = self.cur_token.clone();

        while !self.cur_token_is(TokenType::Semicolon) {
            // println!("Found semicolon");
            self.next_token();
        }

        Some(ReturnStatement {
            token,
            return_value: Box::new(DummyExpression {}),
        })
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn next_token_is(&self, token_type: TokenType) -> bool {
        self.next_token.token_type == token_type
    }

    fn expect_next(&mut self, token_type: TokenType) -> bool {
        if self.next_token_is(token_type.clone()) {
            self.next_token();

            return true;
        }

        self.peek_error(token_type);

        return false;
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let msg = format!("Expected token to be {}, got {}", token_type, self.next_token.token_type);

        self.errors.push(msg)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Node, ReturnStatement};

    use super::*;

    #[test]
    fn let_statement() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
        check_parser_errors(&parser);

        if program.statements.len() != 3 {
            panic!("Program has not 3 statements, got {}", program.statements.len())
        }

        let expected_identifiers = vec![
            "x", "y", "foobar",
        ];

        for i in 0..expected_identifiers.len() {
            let name = expected_identifiers[i];
            let statement = program.statements[i].as_ref();

            assert_let_statement(statement, name);
        }
    }

    #[test]
    fn return_statement() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
        check_parser_errors(&parser);

        if program.statements.len() != 3 {
            panic!("Program has not 3 statements, got {}", program.statements.len())
        }

        for stmt in program.statements {
            let _ = stmt.as_any().downcast_ref::<ReturnStatement>()
                .unwrap_or_else(|| { panic!("Not a \"return\" statement") });

            if stmt.token_literal() != "return" {
                panic!("Not a \"return\" statement, got {}", stmt.token_literal())
            }
        }
    }

    fn assert_let_statement(stmt: &dyn Statement, name: &str) {
        if stmt.token_literal() != "let" {
            panic!("Not a \"let\" statement, got {}", stmt.token_literal())
        }

        let let_statement = stmt.as_any().downcast_ref::<LetStatement>()
            .unwrap_or_else(|| { panic!("Not a \"let\" statement") });
        if let_statement.name.value != name {
            panic!("LET statement name value not {}, got {}", let_statement.name.value, name)
        }
        if let_statement.name.token_literal() != name {
            panic!("LET statement name token literal not {}, got {}", let_statement.name.value, name)
        }
    }

    fn check_parser_errors(parser: &Parser) {
        println!("{} errors", parser.errors.len());

        for error in parser.errors.iter() {
            println!("Parser error: {}", error);
        }

        assert_eq!(parser.errors.len(), 0);
    }
}