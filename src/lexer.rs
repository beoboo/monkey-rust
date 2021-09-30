use crate::token::*;

pub(crate) struct Lexer {
    input: String,
    position: usize,
}

impl Lexer {
    pub(crate) fn new(input: &str) -> Self {
        Self {
            input: input.to_string(),
            position: 0,
        }
    }

    pub(crate) fn next_token(&mut self) -> Token {
        let mut ch = self.read_next();

        while is_whitespace(ch) {
            // println!("Skipping \"{}\"", ch);
            ch = self.read_next();
        }
        // println!("ch: \"{}\"", ch);

        match ch {
            '=' => {
                if self.peek_next() == '=' {
                    let next = self.read_next();
                    let mut str = ch.to_string();
                    str.push(next);

                    Token::new(TokenType::Eq, str.as_str())
                } else {
                    Token::new(TokenType::Assign, ch.to_string().as_str())
                }
            },
            '!' => {
                if self.peek_next() == '=' {
                    let next = self.read_next();
                    let mut str = ch.to_string();
                    str.push(next);

                    Token::new(TokenType::NotEq, str.as_str())
                } else {
                    Token::new(TokenType::Bang, ch.to_string().as_str())
                }
            },
            '+' => Token::new(TokenType::Plus, ch.to_string().as_str()),
            '-' => Token::new(TokenType::Minus, ch.to_string().as_str()),
            '/' => Token::new(TokenType::Slash, ch.to_string().as_str()),
            '*' => Token::new(TokenType::Asterisk, ch.to_string().as_str()),
            '(' => Token::new(TokenType::LParen, ch.to_string().as_str()),
            ')' => Token::new(TokenType::RParen, ch.to_string().as_str()),
            '{' => Token::new(TokenType::LBrace, ch.to_string().as_str()),
            '}' => Token::new(TokenType::RBrace, ch.to_string().as_str()),
            ',' => Token::new(TokenType::Comma, ch.to_string().as_str()),
            ';' => Token::new(TokenType::Semicolon, ch.to_string().as_str()),
            '<' => Token::new(TokenType::Lt, ch.to_string().as_str()),
            '>' => Token::new(TokenType::Gt, ch.to_string().as_str()),
            '\0' => Token::new(TokenType::EOF, ""),
            _ => {
                if is_alpha(ch) {
                    // println!("alpha");
                    let literal = self.read_identifier();
                    Token::new(Token::lookup_keyword(literal), literal)
                } else if is_digit(ch) {
                    // println!("digit");
                    Token::new(TokenType::Number, self.read_number())
                } else {
                    // println!("illegal: {}", ch);
                    Token::new(TokenType::Illegal, ch.to_string().as_str())
                }
            }
        }
    }

    fn read_next(&mut self) -> char {
        if self.position >= self.input.len() {
            return '\0';
        }

        let ch = self.read_char(self.position);
        self.position += 1;
        ch
    }

    fn peek_next(&self) -> char {
        if self.position >= self.input.len() {
            return '\0';
        }

        self.read_char(self.position)
    }

    fn read_identifier(&mut self) -> &str {
        let mut position = self.position;

        let mut ch = self.read_char(position);
        while is_alpha(ch) {
            // println!("{}", ch);
            position += 1;
            ch = self.read_char(position);
        }

        let ident = self.input[(self.position - 1)..position].as_ref();
        // println!("Ident: \"{}\"", ident);
        self.position = position;
        ident
    }

    fn read_number(&mut self) -> &str {
        let mut position = self.position;

        let mut ch = self.read_char(position);
        while is_digit(ch) {
            position += 1;
            ch = self.read_char(position);
        }

        let number = self.input[(self.position - 1)..position].as_ref();
        self.position = position;
        number
    }

    fn read_char(&self, position: usize) -> char {
        self.input.chars().nth(position).unwrap()
    }
}

fn is_alpha(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false
    }
}

fn is_digit(ch: char) -> bool {
    match ch {
        '0'..='9' => true,
        _ => false
    }
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\n' | '\t' | '\r' => true,
        _ => false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operators() {
        let input = "=+(){},;";
        let expected = vec![
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for item in expected {
            let token = lexer.next_token();

            assert_eq!(token.token_type, item.token_type);
            assert_eq!(token.literal, item.literal);
        }
    }

    #[test]
    fn next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
";

        let expected = vec![
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "five"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Number, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "ten"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Number, "10"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "add"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Function, "fn"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "result"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Ident, "add"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Ident, "five"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "ten"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Bang, "!"),
            Token::new(TokenType::Minus, "-"),
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Asterisk, "*"),
            Token::new(TokenType::Number, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Number, "5"),
            Token::new(TokenType::Lt, "<"),
            Token::new(TokenType::Number, "10"),
            Token::new(TokenType::Gt, ">"),
            Token::new(TokenType::Number, "5"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::If, "if"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Number, "5"),
            Token::new(TokenType::Lt, "<"),
            Token::new(TokenType::Number, "10"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::True, "true"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Else, "else"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::False, "false"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Number, "10"),
            Token::new(TokenType::Eq, "=="),
            Token::new(TokenType::Number, "10"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::Number, "10"),
            Token::new(TokenType::NotEq, "!="),
            Token::new(TokenType::Number, "9"),
            Token::new(TokenType::Semicolon, ";"),
            Token::new(TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for item in expected {
            let token = lexer.next_token();
            println!("{:?}", token);

            assert_eq!(token.token_type, item.token_type);
            assert_eq!(token.literal, item.literal);
        }
    }
}