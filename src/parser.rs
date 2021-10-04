use std::collections::HashMap;

use crate::ast::{DummyExpression, Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

type InfixFn = fn(parser: &mut Parser, expr: Box<dyn Expression>) -> Option<Box<dyn Expression>>;
type PrefixFn = fn(parser: &mut Parser) -> Option<Box<dyn Expression>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    next_token: Token,
    errors: Vec<String>,
    prefix_fns: HashMap<TokenType, PrefixFn>,
    infix_fns: HashMap<TokenType, InfixFn>,
    precedences: HashMap<TokenType, Precedence>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let next_token = lexer.next_token();

        let prefix_fns = HashMap::new();
        let infix_fns = HashMap::new();

        let precedences: HashMap<TokenType, Precedence> = vec![
            (TokenType::Eq, Precedence::Equals),
            (TokenType::NotEq, Precedence::Equals),
            (TokenType::Lt, Precedence::LessGreater),
            (TokenType::Gt, Precedence::LessGreater),
            (TokenType::Plus, Precedence::Sum),
            (TokenType::Minus, Precedence::Sum),
            (TokenType::Slash, Precedence::Product),
            (TokenType::Asterisk, Precedence::Product),
        ].into_iter()
            .collect();

        let mut parser = Self { lexer: lexer, cur_token, next_token, errors: vec![], prefix_fns, infix_fns, precedences };

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);
        parser.register_prefix(TokenType::Integer, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);

        parser.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Lt, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Gt, Parser::parse_infix_expression);

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, prefix_fn: PrefixFn) {
        self.prefix_fns.insert(token_type, prefix_fn);
    }

    fn register_infix(&mut self, token_type: TokenType, infix_fn: InfixFn) {
        self.infix_fns.insert(token_type, infix_fn);
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
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
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
            self.next_token();
        }

        Some(Box::new(LetStatement {
            token: let_token,
            name: identifier,
            value: Box::new(DummyExpression {}),
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(ReturnStatement {
            token,
            return_value: Box::new(DummyExpression {}),
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => {
                let msg = format!("Invalid expression for {:?}", token.token_type);

                self.errors.push(msg);

                return None;
            }
        };

        if self.next_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(ExpressionStatement {
            token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let token_type = self.cur_token.token_type;
        let prefix = match self.prefix_fns.get(&token_type) {
            Some(prefix) => prefix,
            None => {
                self.no_prefix_fn_error(&token_type);

                return None;
            }
        };

        let mut left_expr = prefix(self);

        while !self.next_token_is(TokenType::Semicolon) && precedence < self.next_precedence() {
            let token_type = self.next_token.token_type;
            let infix = match self.infix_fns.get(&token_type) {
                Some(infix) => infix.clone(),
                None => return left_expr
            };

            self.next_token();

            let expr: Box<dyn Expression> = left_expr.unwrap();

            left_expr = infix(self, expr);
        }

        left_expr
    }

    fn no_prefix_fn_error(&mut self, token_type: &TokenType) {
        let msg = format!("Could not find a prefix function for {}", token_type);

        self.errors.push(msg);
    }

    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        Some(Box::new(Identifier {
            token: token.clone(),
            value: token.literal,
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        // println!("{}", token.literal);
        let number = match token.literal.parse::<i64>() {
            Ok(n) => n,
            Err(_) => {
                let msg = format!("Could not parse {} as an integer", token.literal);

                self.errors.push(msg);
                return None;
            }
        };

        Some(Box::new(IntegerLiteral {
            token: token.clone(),
            value: number,
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let operator = token.clone().literal;

        self.next_token();

        let right = match self.parse_expression(Precedence::Prefix) {
            Some(expr) => expr,
            None => {
                let msg = format!("Could not parse expression");

                self.errors.push(msg);
                return None;
            }
        };

        Some(Box::new(PrefixExpression {
            token,
            operator,
            right,
        }))
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let operator = token.clone().literal;

        let precedence = self.cur_precedence();

        self.next_token();

        let right = match self.parse_expression(precedence) {
            Some(expr) => expr,
            None => {
                let msg = format!("Could not parse expression");

                self.errors.push(msg);
                return None;
            }
        };

        Some(Box::new(InfixExpression {
            token,
            left,
            operator,
            right,
        }))
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

    fn next_precedence(&self) -> Precedence {
        if self.precedences.contains_key(&self.next_token.token_type) {
            self.precedences[&self.next_token.token_type].clone()
        } else {
            Precedence::Lowest
        }
    }

    fn cur_precedence(&self) -> Precedence {
        if self.precedences.contains_key(&self.cur_token.token_type) {
            self.precedences[&self.cur_token.token_type].clone()
        } else {
            Precedence::Lowest
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{ExpressionStatement, InfixExpression, Node, PrefixExpression, ReturnStatement};

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

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!("Program has not 1 statements, got {}", program.statements.len())
        }

        let stmt = program.statements[0].as_any().downcast_ref::<ExpressionStatement>()
            .unwrap_or_else(|| { panic!("Not an expression statement") });

        let identifier = stmt.expression.as_any().downcast_ref::<Identifier>()
            .unwrap_or_else(|| { panic!("Not an identifier") });

        assert_eq!(identifier.value, "foobar");
        assert_eq!(identifier.token_literal(), "foobar");
    }

    #[test]
    fn integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!("Program has not 1 statements, got {}", program.statements.len())
        }

        let stmt = program.statements[0].as_any().downcast_ref::<ExpressionStatement>()
            .unwrap_or_else(|| { panic!("Not an expression statement") });

        let literal = stmt.expression.as_any().downcast_ref::<IntegerLiteral>()
            .unwrap_or_else(|| { panic!("Not an integer") });

        assert_eq!(literal.value, 5);
        assert_eq!(literal.token_literal(), "5");
    }

    #[test]
    fn parse_prefix_expressions() {
        struct Test<'a> {
            input: &'a str,
            operator: &'a str,
            value: i64,
        }

        let tests = vec![
            Test { input: "!5", operator: "!", value: 5 },
            Test { input: "-15", operator: "-", value: 15 },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);

            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
            check_parser_errors(&parser);

            if program.statements.len() != 1 {
                panic!("Program has not 1 statements, got {}", program.statements.len())
            }

            let stmt = program.statements[0].as_any().downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| { panic!("Not an expression statement") });

            let expr = stmt.expression.as_any().downcast_ref::<PrefixExpression>()
                .unwrap_or_else(|| { panic!("Not a prefix expression") });

            assert_eq!(&expr.operator, test.operator);
            assert_integer_literal(&expr.right, test.value);
        }
    }

    #[test]
    fn parse_infix_expressions() {
        struct Test<'a> {
            input: &'a str,
            left: i64,
            operator: &'a str,
            right: i64,
        }

        let tests = vec![
            Test { input: "5 + 5", left: 5, operator: "+", right: 5 },
            Test { input: "5 - 5", left: 5, operator: "-", right: 5 },
            Test { input: "5 / 5", left: 5, operator: "/", right: 5 },
            Test { input: "5 * 5", left: 5, operator: "*", right: 5 },
            Test { input: "5 > 5", left: 5, operator: ">", right: 5 },
            Test { input: "5 < 5", left: 5, operator: "<", right: 5 },
            Test { input: "5 == 5", left: 5, operator: "==", right: 5 },
            Test { input: "5 != 5", left: 5, operator: "!=", right: 5 },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);

            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
            check_parser_errors(&parser);

            if program.statements.len() != 1 {
                panic!("Program has not 1 statements, got {}", program.statements.len())
            }

            let stmt = program.statements[0].as_any().downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| { panic!("Not an expression statement") });

            let expr = stmt.expression.as_any().downcast_ref::<InfixExpression>()
                .unwrap_or_else(|| { panic!("Not an infix expression") });

            assert_integer_literal(&expr.left, test.left);
            assert_eq!(&expr.operator, test.operator);
            assert_integer_literal(&expr.right, test.right);
        }
    }

    #[test]
    fn operator_precedence() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let tests = vec![
            Test { input: "-a * b", expected: "((-a) * b)" },
            Test { input: "!-a", expected: "(!(-a))" },
            Test { input: "a + b + c", expected: "((a + b) + c)" },
            Test { input: "a * b * c", expected: "((a * b) * c)" },
            Test { input: "a * b / c", expected: "((a * b) / c)" },
            Test { input: "a + b / c", expected: "(a + (b / c))" },
            Test { input: "a + b * c + d / e - f", expected: "(((a + (b * c)) + (d / e)) - f)" },
            Test { input: "3 + 4; -5 * 5", expected: "(3 + 4)((-5) * 5)" },
            Test { input: "5 > 4 == 3 < 4", expected: "((5 > 4) == (3 < 4))" },
            Test { input: "5 < 4 != 3 > 4", expected: "((5 < 4) != (3 > 4))" },
            Test { input: "3 + 4 * 5 == 3 * 1 + 4 * 5", expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);

            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
            check_parser_errors(&parser);

            assert_eq!(format!("{}", program), test.expected);
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

    fn assert_integer_literal(expr: &Box<dyn Expression>, value: i64) {
        let literal = expr.as_any().downcast_ref::<IntegerLiteral>()
            .unwrap_or_else(|| { panic!("Not an integer literal") });

        assert_eq!(literal.value, value);
        assert_eq!(literal.token_literal(), format!("{}", literal.value));
    }

    fn check_parser_errors(parser: &Parser) {
        println!("{} errors", parser.errors.len());

        for error in parser.errors.iter() {
            println!("Parser error: {}", error);
        }

        assert_eq!(parser.errors.len(), 0);
    }
}