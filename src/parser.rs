use std::collections::HashMap;

use crate::ast::*;
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
    Index,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    next_token: Token,
    pub errors: Vec<String>,
    prefix_fns: HashMap<TokenType, PrefixFn>,
    infix_fns: HashMap<TokenType, InfixFn>,
    precedences: HashMap<TokenType, Precedence>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
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
            (TokenType::LParen, Precedence::Call),
            (TokenType::LBracket, Precedence::Index),
        ].into_iter()
            .collect();

        let mut parser = Self { lexer: lexer, cur_token, next_token, errors: vec![], prefix_fns, infix_fns, precedences };

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);
        parser.register_prefix(TokenType::Integer, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::String, Parser::parse_string_literal);
        parser.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::True, Parser::parse_boolean_literal);
        parser.register_prefix(TokenType::False, Parser::parse_boolean_literal);
        parser.register_prefix(TokenType::LParen, Parser::parse_grouped_expression);
        parser.register_prefix(TokenType::If, Parser::parse_if_expression);
        parser.register_prefix(TokenType::Function, Parser::parse_function_literal);
        parser.register_prefix(TokenType::LBracket, Parser::parse_array_literal);
        parser.register_prefix(TokenType::LBrace, Parser::parse_map_literal);
        parser.register_prefix(TokenType::Macro, Parser::parse_macro_literal);

        parser.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Lt, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Gt, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LParen, Parser::parse_call_expression);
        parser.register_infix(TokenType::LBracket, Parser::parse_index_expression);

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

    pub fn parse_program(&mut self) -> Option<Box<Program>> {
        let mut statements = vec![];

        while !self.cur_token_is(TokenType::EOF) {
            let statement = self.parse_statement();

            match statement {
                Some(stmt) => statements.push(stmt),
                _ => {}
            }

            self.next_token();
        }

        Some(Box::new(Program {
            statements
        }))
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
        let name = Identifier { token: id_token.clone(), value: id_token.literal };

        if !self.expect_next(TokenType::Assign) {
            return None;
        }

        self.next_token();

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None
        };

        if self.next_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(LetStatement {
            token: let_token,
            name,
            value,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None
        };

        if self.next_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(ReturnStatement {
            token,
            return_value,
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

        let mut left_expr: Option<Box<dyn Expression>> = prefix(self);

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

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_next(TokenType::RParen) {
            return None;
        }

        expr
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        if !self.expect_next(TokenType::LParen) {
            return None;
        }
        self.next_token();

        let condition = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => {
                let msg = format!("Invalid expression for {:?}", token.token_type);

                self.errors.push(msg);

                return None;
            }
        };

        if !self.expect_next(TokenType::RParen) {
            return None;
        }

        if !self.expect_next(TokenType::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();
        let mut alternative = None;

        if self.next_token_is(TokenType::Else) {
            self.next_token();

            if !self.expect_next(TokenType::LBrace) {
                return None;
            }

            alternative = Some(self.parse_block_statement());
        }

        Some(Box::new(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.cur_token.clone();
        let mut statements = vec![];

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::EOF) {
            match self.parse_statement() {
                Some(stmt) => statements.push(stmt),
                None => {}
            }
            self.next_token();
        }

        BlockStatement {
            token,
            statements,
        }
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        if !self.expect_next(TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_parameters();

        if !self.expect_next(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Box::new(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    fn parse_parameters(&mut self) -> Vec<Identifier> {
        fn _build_identifier(token: Token) -> Identifier {
            Identifier {
                token: token.clone(),
                value: token.literal,
            }
        }

        let mut identifiers = vec![];

        if self.next_token_is(TokenType::RParen) {
            self.next_token();
            return identifiers;
        }

        self.next_token();
        identifiers.push(_build_identifier(self.cur_token.clone()));

        while self.next_token_is(TokenType::Comma) {
            self.next_token();

            self.next_token();
            identifiers.push(_build_identifier(self.cur_token.clone()));
        }

        if !self.expect_next(TokenType::RParen) {
            return vec![];
        }

        identifiers
    }

    fn parse_call_expression(&mut self, function: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let arguments = self.parse_expression_list(TokenType::RParen);

        Some(Box::new(CallExpression {
            token,
            function,
            arguments,
        }))
    }

    fn parse_array_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        let elements = self.parse_expression_list(TokenType::RBracket);

        Some(Box::new(ArrayLiteral {
            token,
            elements,
        }))
    }

    fn parse_map_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        let mut pairs = HashMap::new();

        while !self.next_token_is(TokenType::RBrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;
            if !self.expect_next(TokenType::Colon) {
                return None;
            }

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.insert(key, value);

            if !self.next_token_is(TokenType::RBrace) && !self.expect_next(TokenType::Comma) {
                return None;
            }
        }

        if !self.expect_next(TokenType::RBrace) {
            return None;
        }

        Some(Box::new(MapLiteral {
            token,
            pairs,
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Box<dyn Expression>> {
        let mut arguments = vec![];

        if self.next_token_is(end) {
            self.next_token();
            return arguments;
        }

        self.next_token();
        match self.parse_expression(Precedence::Lowest) {
            Some(expr) => arguments.push(expr),
            None => {}
        }

        while self.next_token_is(TokenType::Comma) {
            self.next_token();

            self.next_token();
            match self.parse_expression(Precedence::Lowest) {
                Some(expr) => arguments.push(expr),
                None => {}
            }
        }

        if !self.expect_next(end) {
            return vec![];
        }

        arguments
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

    fn parse_string_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        Some(Box::new(StringLiteral {
            token: token.clone(),
            value: token.literal,
        }))
    }

    fn parse_boolean_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let boolean = match token.literal.parse::<bool>() {
            Ok(n) => n,
            Err(_) => {
                let msg = format!("Could not parse {} as an integer", token.literal);

                self.errors.push(msg);
                return None;
            }
        };

        Some(Box::new(BooleanLiteral {
            token: token.clone(),
            value: boolean,
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

    fn parse_index_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        self.next_token();

        let index = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None
        };

        if !self.expect_next(TokenType::RBracket) {
            return None;
        }

        Some(Box::new(IndexExpression {
            token,
            left,
            index,
        }))
    }

    fn parse_macro_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        if !self.expect_next(TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_parameters();

        if !self.expect_next(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Box::new(MacroLiteral {
            token,
            parameters,
            body,
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
    use std::ops::Deref;

    use super::*;

    #[test]
    fn parse_let_statement() {
        assert_let_statement("let x = 5;", "x", 5);
        assert_let_statement("let y = true;", "y", true);
        assert_let_statement("let foobar = y;", "foobar", "y");
    }

    fn assert_let_statement<T: ToString>(input: &str, name: &str, value: T) {
        let program = build_program(input);
        assert_program_statements(&program, 1);

        let stmt = program.statements[0].as_any().downcast_ref::<LetStatement>()
            .unwrap_or_else(|| { panic!("Not a \"let\" statement") });

        if stmt.token_literal() != "let" {
            panic!("Not a \"let\" statement, got {}", stmt.token_literal())
        }

        let let_statement = stmt as &LetStatement;

        if let_statement.name.value != name {
            panic!("\"let\" statement name value not {}, got {}", let_statement.name.value, name)
        }

        if let_statement.name.token_literal() != name {
            panic!("\"let\" statement name token literal not {}, got {}", let_statement.name.value, name)
        }

        assert_literal(&*let_statement.value, value)
    }

    #[test]
    fn parse_return_statement() {
        assert_return_statement("return 5;", 5);
        assert_return_statement("return true;", true);
        assert_return_statement("return foobar;", "foobar");
    }

    fn assert_return_statement<T: ToString>(input: &str, value: T) {
        let program = build_program(input);
        assert_program_statements(&program, 1);

        let stmt = program.statements[0].as_any().downcast_ref::<ReturnStatement>()
            .unwrap_or_else(|| { panic!("Not a \"return\" statement") });

        if stmt.token_literal() != "return" {
            panic!("Not a \"return\" statement, got {}", stmt.token_literal())
        }

        assert_literal(&*stmt.return_value, value);
    }

    #[test]
    fn parse_identifier() {
        let input = "foobar;";

        let program = build_program(input);
        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        assert_identifier(&*stmt.expression, "foobar");
    }

    #[test]
    fn parse_integer_literal() {
        let input = "5;";

        let program = build_program(input);
        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        assert_integer_literal(&*stmt.expression, 5);
    }

    struct PrefixTest<'a, T> {
        input: &'a str,
        operator: &'a str,
        value: T,
    }

    #[test]
    fn parse_integer_prefix_expressions() {
        let tests = vec![
            PrefixTest { input: "!5", operator: "!", value: 5 },
            PrefixTest { input: "-15", operator: "-", value: 15 },
        ];

        assert_prefix_expressions(tests);
    }

    #[test]
    fn parse_string_prefix_expressions() {
        let tests = vec![
            PrefixTest { input: "!foobar", operator: "!", value: "foobar" },
            PrefixTest { input: "-foobar", operator: "-", value: "foobar" },
        ];

        assert_prefix_expressions(tests);
    }

    #[test]
    fn parse_boolean_prefix_expressions() {
        let tests = vec![
            PrefixTest { input: "!true", operator: "!", value: true },
            PrefixTest { input: "!false", operator: "!", value: false },
        ];

        assert_prefix_expressions(tests);
    }

    struct InfixTest<'a, T> {
        input: &'a str,
        left: T,
        operator: &'a str,
        right: T,
    }

    #[test]
    fn parse_integer_infix_expressions() {
        let tests = vec![
            InfixTest { input: "5 + 5", left: 5, operator: "+", right: 5 },
            InfixTest { input: "5 - 5", left: 5, operator: "-", right: 5 },
            InfixTest { input: "5 / 5", left: 5, operator: "/", right: 5 },
            InfixTest { input: "5 * 5", left: 5, operator: "*", right: 5 },
            InfixTest { input: "5 > 5", left: 5, operator: ">", right: 5 },
            InfixTest { input: "5 < 5", left: 5, operator: "<", right: 5 },
            InfixTest { input: "5 == 5", left: 5, operator: "==", right: 5 },
            InfixTest { input: "5 != 5", left: 5, operator: "!=", right: 5 },
        ];

        assert_infix_expressions(tests);
    }

    #[test]
    fn parse_string_infix_expressions() {
        let tests = vec![
            InfixTest { input: "foobar + barfoo;", left: "foobar", operator: "+", right: "barfoo" },
            InfixTest { input: "foobar - barfoo;", left: "foobar", operator: "-", right: "barfoo" },
            InfixTest { input: "foobar * barfoo;", left: "foobar", operator: "*", right: "barfoo" },
            InfixTest { input: "foobar / barfoo;", left: "foobar", operator: "/", right: "barfoo" },
            InfixTest { input: "foobar > barfoo;", left: "foobar", operator: ">", right: "barfoo" },
            InfixTest { input: "foobar < barfoo;", left: "foobar", operator: "<", right: "barfoo" },
            InfixTest { input: "foobar == barfoo;", left: "foobar", operator: "==", right: "barfoo" },
            InfixTest { input: "foobar != barfoo;", left: "foobar", operator: "!=", right: "barfoo" },
        ];

        assert_infix_expressions(tests);
    }

    #[test]
    fn parse_boolean_infix_expressions() {
        let tests = vec![
            InfixTest { input: "true == true;", left: true, operator: "==", right: true },
            InfixTest { input: "true != false;", left: true, operator: "!=", right: false },
            InfixTest { input: "false == false;", left: false, operator: "==", right: false },
        ];

        assert_infix_expressions(tests);
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
            Test { input: "true", expected: "true" },
            Test { input: "false", expected: "false" },
            Test { input: "3 > 5 == false", expected: "((3 > 5) == false)" },
            Test { input: "3 < 5 == true", expected: "((3 < 5) == true)" },
            Test { input: "1 + (2 + 3) + 4", expected: "((1 + (2 + 3)) + 4)" },
            Test { input: "(5 + 5) * 2", expected: "((5 + 5) * 2)" },
            Test { input: "2 / (5 + 5)", expected: "(2 / (5 + 5))" },
            Test { input: "(5 + 5) * 2 * (5 + 5)", expected: "(((5 + 5) * 2) * (5 + 5))" },
            Test { input: "-(5 + 5)", expected: "(-(5 + 5))" },
            Test { input: "!(true == true)", expected: "(!(true == true))" },
            Test { input: "a + add(b * c) + d", expected: "((a + add((b * c))) + d)" },
            Test { input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
            Test { input: "a * [1, 2, 3, 4][b * c] * d", expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)" },
            Test { input: "add(a * b[2], b[1], 2 * [1, 2][1])", expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))" },
        ];

        for test in tests {
            let program = build_program(test.input);

            assert_eq!(format!("{}", program), test.expected);
        }
    }

    #[test]
    fn parse_boolean_literal() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }

        let tests = vec![
            Test { input: "true;", expected: true },
            Test { input: "false;", expected: false },
        ];

        for test in tests {
            let program = build_program(test.input);

            let stmt = parse_expression_statement(&*program.statements[0]);

            assert_boolean_literal(&*stmt.expression, test.expected);
        }
    }

    #[test]
    fn parse_string_literal() {
        let input = "\"hello world\"";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let string = stmt.expression.as_any().downcast_ref::<StringLiteral>()
            .unwrap_or_else(|| { panic!("Not a string literal") });

        assert_eq!(string.value.as_str(), "hello world")
    }

    #[test]
    fn parse_if_expression() {
        let input = "if (x < y) { x }";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let if_expr = stmt.expression.as_any().downcast_ref::<IfExpression>()
            .unwrap_or_else(|| { panic!("Not an if expression literal") });

        assert_infix_expression(&*if_expr.condition, "x", "<", "y");
        assert_block_statement(&if_expr.consequence, 1, "x");
        assert!(if_expr.alternative.is_none());
    }

    #[test]
    fn parse_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let if_expr = stmt.expression.as_any().downcast_ref::<IfExpression>()
            .unwrap_or_else(|| { panic!("Not an if expression literal") });

        assert_infix_expression(&*if_expr.condition, "x", "<", "y");
        assert_block_statement(&if_expr.consequence, 1, "x");
        assert_block_statement(if_expr.alternative.as_ref().unwrap(), 1, "y");
    }

    #[test]
    fn parse_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let function = stmt.expression.as_any().downcast_ref::<FunctionLiteral>()
            .unwrap_or_else(|| { panic!("Not a function literal") });

        if function.parameters.len() != 2 {
            panic!("Function has not {} parameters, got {}", 2, function.parameters.len())
        }

        assert_identifier(&function.parameters[0], "x");
        assert_identifier(&function.parameters[1], "y");

        let body = &function.body;

        if body.statements.len() != 1 {
            panic!("Body has not {} statements, got {}", 1, body.statements.len())
        }

        let body_stmt = parse_expression_statement(&*body.statements[0]);

        assert_infix_expression(&*body_stmt.expression, "x", "+", "y");
    }

    #[test]
    fn parse_parameters() {
        struct Test<'a> {
            input: &'a str,
            expected: Vec<&'a str>,
        }

        let tests = vec![
            Test { input: "fn() {}", expected: vec![] },
            Test { input: "fn(x) {}", expected: vec!["x"] },
            Test { input: "fn(x, y, z) {}", expected: vec!["x", "y", "z"] },
        ];

        for test in tests {
            let program = build_program(test.input);

            let stmt = parse_expression_statement(&*program.statements[0]);
            let function = stmt.expression.as_any().downcast_ref::<FunctionLiteral>()
                .unwrap_or_else(|| { panic!("Not a function literal") });

            assert_eq!(function.parameters.len(), test.expected.len());


            for i in 0..test.expected.len() {
                assert_literal(&function.parameters[i], test.expected[i]);
            }
        }
    }

    #[test]
    fn parse_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let call = stmt.expression.as_any().downcast_ref::<CallExpression>()
            .unwrap_or_else(|| { panic!("Not a call expression") });

        assert_identifier(&*call.function, "add");

        if call.arguments.len() != 3 {
            panic!("Call has not {} arguments, got {}", 3, call.arguments.len())
        }

        assert_literal(&*call.arguments[0], 1);
        assert_infix_expression(&*call.arguments[1], 2, "*", 3);
        assert_infix_expression(&*call.arguments[2], 4, "+", 5);
    }

    #[test]
    fn parse_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let array = stmt.expression.as_any().downcast_ref::<ArrayLiteral>()
            .unwrap_or_else(|| { panic!("Not an array literal") });

        if array.elements.len() != 3 {
            panic!("Array has not {} elements, got {}", 3, array.elements.len())
        }

        assert_integer_literal(&*array.elements[0], 1);
        assert_infix_expression(&*array.elements[1], 2, "*", 2);
        assert_infix_expression(&*array.elements[2], 3, "+", 3);
    }

    #[test]
    fn parse_index_expression() {
        let input = "myArray[1 + 1]";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let index = stmt.expression.as_any().downcast_ref::<IndexExpression>()
            .unwrap_or_else(|| { panic!("Not an index expression") });

        assert_identifier(&*index.left, "myArray");
        assert_infix_expression(&*index.index, 1, "+", 1);
    }

    #[test]
    fn parse_empty_hash_literal() {
        let input = "{}";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let hash = stmt.expression.as_any().downcast_ref::<MapLiteral>()
            .unwrap_or_else(|| { panic!("Not an hash literal") });

        if hash.pairs.len() != 0 {
            panic!("Hash has not {} pairs, got {}", 0, hash.pairs.len())
        }
    }

    #[test]
    fn parse_map_literal_string_keys() {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3}";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let hash = stmt.expression.as_any().downcast_ref::<MapLiteral>()
            .unwrap_or_else(|| { panic!("Not an hash literal") });

        if hash.pairs.len() != 3 {
            panic!("Hash has not {} pairs, got {}", 3, hash.pairs.len())
        }

        let expected: HashMap<&str, i64> = vec![
            ("one", 1i64),
            ("two", 2i64),
            ("three", 3i64),
        ].into_iter().collect();

        for (key, value) in &hash.pairs {
            let literal = key.as_any().downcast_ref::<StringLiteral>()
                .unwrap_or_else(|| { panic!("Not a string literal") });

            let expected_value = expected[literal.to_string().as_str()];

            assert_integer_literal(value.deref(), expected_value)
        }
    }

    #[test]
    fn parse_map_literal_boolean_keys() {
        let input = "{true: 1, false: 2}";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let hash = stmt.expression.as_any().downcast_ref::<MapLiteral>()
            .unwrap_or_else(|| { panic!("Not an hash literal") });

        if hash.pairs.len() != 2 {
            panic!("Hash has not {} pairs, got {}", 2, hash.pairs.len())
        }

        let expected: HashMap<&str, i64> = vec![
            ("true", 1i64),
            ("false", 2i64),
        ].into_iter().collect();

        for (key, value) in &hash.pairs {
            let literal = key.as_any().downcast_ref::<BooleanLiteral>()
                .unwrap_or_else(|| { panic!("Not a boolean literal") });

            let expected_value = expected[literal.to_string().as_str()];

            assert_integer_literal(value.deref(), expected_value)
        }
    }

    #[test]
    fn parse_map_literal_integer_keys() {
        let input = "{1: 1, 2: 2, 3: 3}";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let hash = stmt.expression.as_any().downcast_ref::<MapLiteral>()
            .unwrap_or_else(|| { panic!("Not an hash literal") });

        if hash.pairs.len() != 3 {
            panic!("Hash has not {} pairs, got {}", 3, hash.pairs.len())
        }

        let expected: HashMap<&str, i64> = vec![
            ("1", 1i64),
            ("2", 2i64),
            ("3", 3i64),
        ].into_iter().collect();

        for (key, value) in &hash.pairs {
            let literal = key.as_any().downcast_ref::<IntegerLiteral>()
                .unwrap_or_else(|| { panic!("Not an integer literal") });

            let expected_value = expected[literal.to_string().as_str()];

            assert_integer_literal(value.deref(), expected_value)
        }
    }

    #[test]
    fn parse_map_literal_expression_values() {
        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let hash = stmt.expression.as_any().downcast_ref::<MapLiteral>()
            .unwrap_or_else(|| { panic!("Not an hash literal") });

        if hash.pairs.len() != 3 {
            panic!("Hash has not {} pairs, got {}", 3, hash.pairs.len())
        }

        type AssertFn = fn(exp: &dyn Expression);

        let mut expected: HashMap<&str, AssertFn> = HashMap::new();
        expected.insert("one", |exp| assert_infix_expression(exp, 0, "+", 1));
        expected.insert("two", |exp| assert_infix_expression(exp, 10, "-", 8));
        expected.insert("three", |exp| assert_infix_expression(exp, 15, "/", 5));

        for (key, value) in &hash.pairs {
            let literal = key.as_any().downcast_ref::<StringLiteral>()
                .unwrap_or_else(|| { panic!("Not a string literal") });

            let assert_fn = expected.get(literal.to_string().as_str()).unwrap_or_else(|| { panic!("No assert function for {}", literal) });

            assert_fn(value.deref());
        }
    }

    #[test]
    fn parse_macro_literal() {
        let input = "macro(x, y) { x + y; }";

        let program = build_program(input);

        assert_program_statements(&program, 1);

        let stmt = parse_expression_statement(&*program.statements[0]);

        let macro_ = stmt.expression.as_any().downcast_ref::<MacroLiteral>()
            .unwrap_or_else(|| { panic!("Not a macro literal") });

        if macro_.parameters.len() != 2 {
            panic!("Function has not {} parameters, got {}", 2, macro_.parameters.len())
        }

        assert_identifier(&macro_.parameters[0], "x");
        assert_identifier(&macro_.parameters[1], "y");

        let body = &macro_.body;

        if body.statements.len() != 1 {
            panic!("Body has not {} statements, got {}", 1, body.statements.len())
        }

        let body_stmt = parse_expression_statement(&*body.statements[0]);

        assert_infix_expression(&*body_stmt.expression, "x", "+", "y");
    }


    fn assert_block_statement(block_stmt: &BlockStatement, count: usize, ident: &str) {
        if block_stmt.statements.len() != count {
            panic!("Block has not {} statements, got {}", count, block_stmt.statements.len())
        }

        let stmt = parse_expression_statement(&*block_stmt.statements[0]);

        assert_identifier(&*stmt.expression, ident);
    }

    fn assert_program_statements(program: &Program, count: usize) {
        if program.statements.len() != count {
            panic!("Program has not {} statements, got {}", count, program.statements.len())
        }
    }

    fn build_program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_else(|| panic!("Invalid parsed program"));
        check_parser_errors(&parser);

        *program
    }

    fn assert_integer_literal(expr: &dyn Expression, value: i64) {
        let _literal = expr.as_any().downcast_ref::<IntegerLiteral>()
            .unwrap_or_else(|| { panic!("Not an integer literal") });

        assert_literal(expr, value);
    }

    fn assert_boolean_literal(expr: &dyn Expression, value: bool) {
        let _literal = expr.as_any().downcast_ref::<BooleanLiteral>()
            .unwrap_or_else(|| { panic!("Not an boolean literal") });

        assert_literal(expr, value);
    }

    fn assert_identifier(expr: &dyn Expression, value: &str) {
        let _identifier = expr.as_any().downcast_ref::<Identifier>()
            .unwrap_or_else(|| { panic!("Not an identifier") });

        assert_literal(expr, value);
    }

    fn parse_expression_statement(expr_stmt: &dyn Statement) -> &ExpressionStatement {
        expr_stmt.as_any().downcast_ref::<ExpressionStatement>()
            .unwrap_or_else(|| { panic!("Not an expression statement") })
    }

    fn assert_prefix_expressions<T: ToString>(tests: Vec<PrefixTest<T>>) {
        for test in tests {
            let program = build_program(test.input);
            assert_program_statements(&program, 1);

            let stmt = parse_expression_statement(&*program.statements[0]);

            assert_prefix_expression(&*stmt.expression, test.operator, test.value);
        }
    }

    fn assert_prefix_expression<T: ToString>(expr: &dyn Expression, operator: &str, value: T) {
        let expr = expr.as_any().downcast_ref::<PrefixExpression>()
            .unwrap_or_else(|| { panic!("Not a prefix expression") });

        assert_eq!(&expr.operator, operator);
        assert_literal(&*expr.right, value);
    }

    fn assert_infix_expressions<T: ToString>(tests: Vec<InfixTest<T>>) {
        for test in tests {
            let program = build_program(test.input);
            assert_program_statements(&program, 1);

            let stmt = parse_expression_statement(&*program.statements[0]);

            assert_infix_expression(&*stmt.expression, test.left, test.operator, test.right);
        }
    }

    fn assert_infix_expression<T>(expr: &dyn Expression, left: T, operator: &str, right: T)
        where T: ToString {
        let infix = expr.as_any().downcast_ref::<InfixExpression>()
            .unwrap_or_else(|| { panic!("Not an infix expression") });

        assert_literal(&*infix.left, left);
        assert_eq!(&infix.operator, operator);
        assert_literal(&*infix.right, right);
    }

    fn assert_literal<T: ToString>(expr: &dyn Expression, value: T) {
        assert_eq!(expr.value(), value.to_string());
        assert_eq!(expr.token_literal(), format!("{}", expr.value()));
    }

    fn check_parser_errors(parser: &Parser) {
        println!("{} errors", parser.errors.len());

        for error in parser.errors.iter() {
            println!("Parser error: {}", error);
        }

        assert_eq!(parser.errors.len(), 0);
    }
}